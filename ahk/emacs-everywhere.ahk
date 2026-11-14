; emacs-everywhere.ahk — Windows front-end for tecosaur/emacs-everywhere
; AutoHotkey v2. AHK does the OS work (like xdotool/xclip on X11):
;   capture the focused window -> copy the selection -> open an Emacs frame
;   -> wait for it to close -> refocus the window -> paste.
; Emacs-side config: emacs-everywhere-windows.el
#Requires AutoHotkey v2.0
#SingleInstance Force

; ---- config ---------------------------------------------------------------
EMACSCLIENTW      := "emacsclientw.exe"   ; full path if not on PATH, e.g. "C:\Program Files\Emacs\emacs-30.2\bin\emacsclientw.exe"
TMP_DIR           := A_Temp               ; must equal Emacs's `temporary-file-directory` (default: %TEMP%)
RESTORE_CLIPBOARD := true                 ; put the old clipboard back after pasting
COPY_TIMEOUT      := 0.3                  ; seconds to wait for Ctrl+C to produce a selection
EE_HOTKEY         := "^!e"                ; Ctrl+Alt+E
; Apps where Ctrl+C is NOT copy (terminals: it's SIGINT). Selection grab is skipped.
NO_COPY_EXES      := ["WindowsTerminal.exe", "conhost.exe", "wezterm-gui.exe", "alacritty.exe", "mintty.exe"]

Hotkey EE_HOTKEY, EmacsEverywhere

EmacsEverywhere(*) {
    ; Let go of the hotkey's modifiers first, or Send "^c" becomes Ctrl+Alt+C.
    KeyWait "Ctrl"
    KeyWait "Alt"
    KeyWait "Shift"
    KeyWait "LWin"

    hwnd := WinExist("A")
    if !hwnd
        return
    title := WinGetTitle(hwnd)
    exe   := WinGetProcessName(hwnd)
    cls := RegExReplace(RegExReplace(exe, "i)\.exe$"), "[^\w-]", "_")
    WinGetPos &x, &y, &w, &h, hwnd

    ; Copy the current selection. Clipboard is left holding ONLY the selection
    ; (empty if none) so Emacs can insert it via the clipboard.
    saved := ClipboardAll()
    A_Clipboard := ""
    if !HasValue(NO_COPY_EXES, exe) {
        Send "^c"
        ClipWait COPY_TIMEOUT
    }

    base := TMP_DIR "\emacs-everywhere-" FormatTime(, "yyyyMMdd-HHmmss") "-" cls
    file := base
    meta := base ".app"
    ; Sidecar with window info; read by the Emacs info-function (avoids
    ; escaping titles through command-line + Lisp reader quoting).
    try FileDelete meta
    FileAppend hwnd "`n" cls "`n" title "`n" x " " y " " w " " h "`n", meta, "UTF-8-RAW"

    try {
        ; -c new frame; -a "" starts the daemon if it isn't running.
        ; Blocks until the buffer is finished (C-c C-c) or aborted (C-c C-k).
        RunWait '"' EMACSCLIENTW '" -c -a "" "' file '"'
    } catch as e {
        MsgBox "emacsclientw failed: " e.Message, "emacs-everywhere"
        A_Clipboard := saved
        return
    }

    ; Emacs writes the file only on a non-aborted finish (see the .el config).
    if FileExist(file) {
        text := FileRead(file, "UTF-8")
        A_Clipboard := text
        ClipWait 1
        WinActivate hwnd
        if WinWaitActive(hwnd, , 1) {
            Send "^v"
            Sleep 200   ; let the target app read the clipboard before we restore it
            if RESTORE_CLIPBOARD
                A_Clipboard := saved
        } else {
            ; Leave the edited text on the clipboard so nothing is lost.
            TrayTip "Couldn't refocus the window; text is on the clipboard.", "emacs-everywhere"
        }
        try FileDelete file
    } else {
        A_Clipboard := saved
        WinActivate hwnd
    }
    try FileDelete meta
}

HasValue(arr, v) {
    for x in arr
        if (x = v)   ; case-insensitive
            return true
    return false
}

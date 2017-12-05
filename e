#!/bin/bash
# An emacs 'alias' with the ability to read from stdin
# If the argument is - then write stdin to a tempfile and open the
# tempfile.
if [[ $# -ge 1 ]] && [[ "$1" == - ]]; then
	tempfile="$(mktemp emacs-stdin-$USER.XXXXXXX --tmpdir)"
	cat - > "$tempfile"
	emacsclient -c --eval "(find-file \"$tempfile\")" \
	      --eval '(set-visited-file-name nil)' \
	      --eval '(rename-buffer "*stdin*" t)'
else
	emacs "$@"
fi

export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"

alias p2x1="pdfnup --nup 2x1 --landscape --suffix '2x1' --batch "

pdfconstcrop() {
    pdfcrop --bbox "$(
        pdfcrop --verbose "$@" |
        grep '^%%HiResBoundingBox: ' |
        cut -d' ' -f2- |
        datamash -t' ' min 1 min 2 max 3 max 4
    )" "$@"
}

pdfcrop_all() {
    for FILE in *.pdf; do
        pdfconstcrop --margins '20 20 20 20' "${FILE}"
    done
}

eval "$(direnv hook bash)"
eval "$(starship init bash)"

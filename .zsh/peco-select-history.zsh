function peco-select-history() {
    local tac
    if which tac > /dev/null; then
        tac="tac"
    else
        tac="tail -r"
    fi
    BUFFER=$(\history -n 1 | eval $tac | awk '!a[$0]++' | peco --query "$LBUFFER")
    CURSOR=$#BUFFER
    # zle clear-screen
}
zle -N peco-select-history
# 履歴からコマンド選択
bindkey '^r' peco-select-history

# 環境変数
export LANG=ja_JP.UTF-8

# 古いMacだといるっぽい
PATH=/bin:/usr/bin:/usr/local/bin:${PATH}
export PATH

if [ -d ${HOME}/.pyenv ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi

# 色を使用出来るようにする
autoload -Uz colors
colors

# 補完機能を有効にする
autoload -Uz compinit
compinit

# cd したら自動的にpushdする
setopt auto_pushd
# 重複したディレクトリを追加しない
setopt pushd_ignore_dups

# 同時に起動したzshの間でヒストリを共有する
setopt share_history

# 同じコマンドをヒストリに残さない
setopt hist_ignore_all_dups

# ヒストリの設定
HISTFILE=~/.zsh_history
HISTSIZE=1000000
SAVEHIST=1000000

# プロンプト
# 1行表示
# PROMPT="%~ %# "
# 2行表示
PROMPT="%{${fg[green]}%}[%n@%m]%{${reset_color}%} %~
%# "

########################################
# vcs_info
autoload -Uz add-zsh-hook
autoload -Uz vcs_info
setopt PROMPT_SUBST
zstyle ':vcs_info:*' formats '(%s)-[%b] %m'  # hook_com[misc]を出力するため%mを追加
zstyle ':vcs_info:*' actionformats '(%s)-[%b|%a] %m'
_vcs_precmd () { vcs_info }
add-zsh-hook precmd _vcs_precmd
RPROMPT='%F{green}${vcs_info_msg_0_}%f'

# gitリポジトリにいる場合、set-messageフックでgit-config-user関数が呼び出されるように登録
zstyle ':vcs_info:git+set-message:*' hooks git-config-user

# "+vi-<フック名>"関数で、hook_com[misc]にgit config user.emailの結果を代入する
function +vi-git-config-user(){
  hook_com[misc]+=`git config user.email`
}



# emacs 風キーバインドにする
bindkey -e

# 日本語ファイル名を表示可能にする
setopt print_eight_bit

# beep を無効にする
setopt no_beep

# フローコントロールを無効にする
setopt no_flow_control

# Ctrl+Dでzshを終了しない
setopt ignore_eof

# '#' 以降をコメントとして扱う
setopt interactive_comments

# launch emacs on commandline
alias emacs="/Applications/Emacs.app/Contents/MacOS/Emacs -nw"

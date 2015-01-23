# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# new PATH
export PATH=/opt/java/bin:/usr/local/texlive/2011/bin/x86_64-linux:~/bin:~/proj/work/raysconfig/bin:~/proj/bin:$PATH

# ibus
export GTK_IM_MODULE=ibus
export XMODIFIERS=@im=ibus
export QT_IM_MODULE=ibus
export XIM=ibus

# colorful man page
export PAGER="`which less` -s"
export BROWSER="$PAGER"
export LESS_TERMCAP_mb=$'\E[01;34m'
export LESS_TERMCAP_md=$'\E[01;34m'
export LESS_TERMCAP_me=$'\E[0m'
export LESS_TERMCAP_se=$'\E[0m'
export LESS_TERMCAP_so=$'\E[01;44;33m'
export LESS_TERMCAP_ue=$'\E[0m'
export LESS_TERMCAP_us=$'\E[01;33m'

# auto complete after sudo and man
complete -cf sudo
complete -cf man

shopt -s cdspell
shopt -s checkwinsize
shopt -s cmdhist
shopt -s dotglob
shopt -s expand_aliases
shopt -s extglob
shopt -s histappend
shopt -s hostcomplete
shopt -s nocaseglob

export HISTSIZE=10000
export HISTFILESIZE=${HISTSIZE}
export HISTCONTROL=ignoreboth

issue=`cat /etc/issue`
[[ "$issue" == *Ubuntu* ]] && . ~/proj/work/raysconfig/.bash_aliases_ubuntu
[[ "$issue" == *Arch* ]] && . ~/proj/work/raysconfig/.bash_aliases_arch

# extract archive
extract() {
    local c e i

    (($#)) || return

    for i; do
        c=''
        e=1

        if [[ ! -r $i ]]; then
            echo "$0: file is unreadable: \`$i'" >&2
            continue
        fi

        case $i in
        *.t@(gz|lz|xz|b@(2|z?(2))|a@(z|r?(.@(Z|bz?(2)|gz|lzma|xz)))))
               c='bsdtar xvf';;
        *.7z)  c='7z x';;
        *.Z)   c='uncompress';;
        *.bz2) c='bunzip2';;
        *.exe) c='cabextract';;
        *.gz)  c='gunzip';;
        *.rar) c='unrar x';;
        *.xz)  c='unxz';;
        *.zip) c='unzip';;
        *)     echo "$0: unrecognized file extension: \`$i'" >&2
               continue;;
        esac

        command $c "$i"
        e=$?З
    done

    return $e
}

# vim stuff
export EDITOR=vim
export VISUAL=vim
alias vi=vim

PS1="\[\033[01;34m\][\u@\h]:\[\033[00;34m\]\w\n$\[\033[00m\]"
export PS1="$PS1"'$([ -n "$TMUX" ] && tmux setenv TMUXPWD_$(tmux display -p "#D" | tr -d %) "$PWD")'

### chsdir start ###
. $HOME/proj/work/raysconfig/bin/chs_completion
#export CHSDIR="{'n':'l'}"
complete -o filenames -F _filedir_xspec file
### chsdir finish. ###

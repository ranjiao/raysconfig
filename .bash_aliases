alias sshc='LC_TYPE=zh_CN.GBK LANG=zh_CN.GBK ssh'
alias svnc='LC_TYPE=zh_CN.UTF8 LANG=zh_CN.UTF8 LC_ALL=zh_CN.UTF8 svn'
alias c7z='7z a -t7z -m0=lzma -mx=8 -md=32m'
alias ojava='optirun java'
alias terminator='terminator --geometry=1024x400+0+0'
alias opera='QT_IM_MODULE=xim XMODIFIERS="@im=ibus" opera'
alias ff='firefox'
alias chx='chmod +x'
alias shut='sudo shutdown -h now'
alias shit='shut'
alias reboot='sudo reboot'
alias c='clear'
alias sd='svn diff --diff-cmd=~/bin/svn-diff-meld'

alias wine='LC_CTYPE=zh_CN.UTF8 LANG=zh_CN.UTF8 wine'
alias emacsc='LC_CTYPE=zh_CN.UTF8 LANG=zh_CN.UTF8 emacs'

#grep alias
alias grep='grep --color=auto'
alias fgrep='grep --color==auto'
alias egrep='grep --color==auto'

alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias grep='grep --color=tty -d skip'
alias cp="cp -i"                          # confirm before overwriting something
alias df='df -h'                          # human-readable sizes
alias free='free -m'                      # show sizes in MB

# convert GBXXX files into utf8
alias convgbk='iconv -f GBK -t utf-8'
alias convgb='iconv -f GB18030 -t utf-8'

alias cmakeit='cmake .. -DCMAKE_INSTALL_PREFIX=${HOME}/proj'

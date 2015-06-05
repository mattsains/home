# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Uncomment the following line if you don't like systemctl's auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

alias ls='ls -lh --color=auto '
alias emacs='emacs -nw'
alias cdiff='git diff --no-index -- '

alias gs='git status'

alias work='cd /home/matt/Varsity/Treatise/Work'
alias bench='cd /home/matt/Varsity/Treatise/benchmarks'
alias register='cd /home/matt/Varsity/Treatise/Work/Virtual\ Machines/Register\ Mapped'

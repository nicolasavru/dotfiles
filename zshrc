# /etc/zsh/zshrc: system-wide .zshrc file for zsh(1)
#
# Originally based on configuration by Calogero Lo Leggio
# <kalos@nerdrug.org>
#
# Part of this configuration has based on various configuration files
# taken from the web...  the most important references are:
# http://strcat.de/zsh/ http://www.jukie.net/~bart/conf/
#
# bash and zsh config file load order:
# http://shreevatsa.wordpress.com/2008/03/30/zshbash-startup-files-loading-order-bashrc-zshrc-etc/
#
# This file is sourced only for interactive shells. It should contain
# commands to set up aliases, functions, options, key bindings, etc.
#
# Global Order: zshenv, zprofile, zshrc, zlogin

# Set the "umask" (see "man umask"):
# umask 002 # relaxed   -rwxrwxr-x
# umask 022 # cautious  -rwxr-xr-x
# umask 027 # uptight   -rwxr-x---
# umask 077 # paranoid  -rwx------
# umask 066 # bofh-like -rw-------
umask 002

# If root set umask to 022 to prevent new files being created group and world writable
#if (( EUID == 0 )); then
#    umask 022
#fi

#export TERM=rxvt-unicode-256color

setopt extended_glob
for zsh_config in ~/.zsh.d/[0-9][0-9]*[^~] ; do
    source $zsh_config
done

eval $( dircolors -b $HOME/LS_COLORS/LS_COLORS )

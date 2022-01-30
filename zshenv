# /etc/zsh/zshenv: system-wide .zshenv file for zsh(1).
#
# This file is sourced on all invocations of the shell.
# If the -f flag is present or if the NO_RCS option is
# set within this file, all other initialization files
# are skipped.
#
# This file should contain commands to set the command
# search path, plus other important environment variables.
# This file should not contain commands that produce
# output or assume the shell is attached to a tty.
#
# Global Order: zshenv, zprofile, zshrc, zlogin

source /etc/profile

export PATH=$HOME/dotfiles/bin:$HOME/bin:$HOME/scripts:$PATH
export PATH=$HOME/.cargo/bin:$PATH

export BROWSER="firefox-nightly"
export EDITOR="emacsclient_launcher.sh"
export VISUAL="emacsclient_launcher.sh"
export ALTERNATE_EDITOR="vim"

export LESS=" -R "
export LESSOPEN="|lesspipe.sh %s"
export PAGER="less"

export WINEDEBUG="-all"
export pacman_program="pikaur"
export VDPAU_DRIVER="va_gl"

export LIBVIRT_DEFAULT_URI="qemu:///system"

export JAVA_HOME=/usr/lib/jvm/default
export PATH=$JAVA_HOME/bin:$PATH
export _JAVA_AWT_WM_NONREPARENTING=1

MOZ_ENABLE_WAYLAND=1

export QT_SCALE_FACTOR=1.25
export QT_QPA_PLATFORMTHEME="qt5ct"
#export GDK_DPI_SCALE=1.25

# Kill flow control
if tty -s ; then
    stty -ixon
    stty -ixoff
fi

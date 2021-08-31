# Source new zsh

# [[file:README.org::*Source new zsh][Source new zsh:1]]
sudo mkdir -p /etc/zsh
cat << EndOfConf | sudo tee /etc/zsh/zshenv
if [[ -z "$XDG_CONFIG_HOME" ]]
then
        export XDG_CONFIG_HOME="$HOME/.config/"
fi

if [[ -d "$XDG_CONFIG_HOME/zsh" ]]
then
        export ZDOTDIR="$XDG_CONFIG_HOME/zsh/"
fi
EndOfConf
# Source new zsh:1 ends here

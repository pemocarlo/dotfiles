[ -d /sys/firmware/efi ] && echo "EFI boot on HDD" || echo "Legacy boot on HDD"
DF
df
sudo mount /dev/sda8 /mnt
sudo unmount /dev/sda8
sudo umount /dev/sda8
sudo apt-add-repository ppa:yannubuntu/boot-repair
sudo apt-get update
sudo apt-get install -y boot-repair
boot-repair&
sudo dpkg --configure -a
sudo apt-get install -fy
sudo apt-get purge -y grub*-common grub-common:i386 shim-signed
sudo apt-get install -y grub-pc os-prober
boot-repair&
sudo reboot
byobu-config 
exit
sudo apt update && sudo apt upgrade
sudo apt install gnome-tweak-tool
gsettings set org.gnome.shell.extensions.dash-to-dock click-action 'minimize'
sudo add-apt-repository ppa:system76/pop
sudo apt install gnome-shell-extensions
sudo apt install ubuntu-restricted-extras
sudo apt autoremove 
sudo apt autoclean 
df
sudo apt install flatpak
sudo add-apt-repository ppa:papirus/papirus
sudo apt update
sudo apt install papirus-icon-theme
sudo apt install tlp tlp-rdw
sudo tlp start
sudo apt autoremove
touch ~/Templates/Empty\ Document
sudo apt dist-upgrade
sudo apt install libavcodec-extra
sudo apt install libdvd-pkg
sudo apt-get install preload
sudo apt autoremove 
sudo apt autoclean 
sudo apt update 
sudo apt upgrade 
sudo apt install preload
sudo apt-get clean
sudo ufw enable
sudo apt install gufw
sudo apt install sophos
sudo apt install vlc
sudo ufw status verbose
sudo apt purge ubuntu-web-launchers
sudo dpkg-reconfigure libdvd-pkg
sudo apt install Lxkeymap
sudo apt install lxkeymap
sudo apt-get install git tar clang ripgrep fd-find
snap install ripgrep
snap install --classic ripgrep
apt-get install git tar clang
SUDO apt-get install git tar clang
sudo apt-get install git tar clang
sudo apt-get install build-essential xz-utils curl
sudo apt install fd
wget https://github.com/sharkdp/fd/releases/download/v7.4.0/fd-musl_7.3.0_amd64.deb
wget https://github.com/sharkdp/fd/releases/download/v7.4.0/fd-musl_7.4.0_amd64.deb
sudo dpkg -i fd-musl_7.4.0_amd64.deb 
fd --version
man fd
add-apt-repository ppa:kelleyk/emacs
sudo add-apt-repository ppa:kelleyk/emacs
sudo apt-get update
sudo apt-get install emacs26
emacs --version
git clone https://github.com/hlissner/doom-emacs ~/.emacs.d
~/.emacs.d/bin/doom install
rm -rf .emacs.d/
rm -rf .doom.d/
sudo apt install libpng-dev zlib1g-dev
sudo apt install libpoppler-glib-dev
sudo aptitude install libpoppler-private-dev
sudo apt install libpoppler-private-dev
sudo apt autoremove 
sudo apt autoclean 
sudo apt install byobu 
byobu
exit
ls
exit
byobu
exit
ls
sudo apt-get install dconf-cli
git clone https://github.com/aruhier/gnome-terminal-colors-solarized.git
cd gnome-terminal-colors-solarized
./install.sh
./set_light.sh 
ls
ls -al
exit
byobu
exit
ls -al
emacs -nwq
emacs -nw -q
ls
./set_light.sh 
cd gnome-terminal-colors-solarized/
./set_light.sh 
ls
cd ..
ls -al
ls
emacs&
byobu --version
sudo apt install font-manager
emacs&
git clone https://github.com/hlissner/doom-emacs ~/doom-emacs
~/doom-emacs/bin/doom install
emacs --with-profile doom &
sudo apt remove clang
curl -SL http://releases.llvm.org/9.0.0/clang%2bllvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04.tar.xz | tar -xJC . 
mv clang+llvm-9.0.0-x86_64-linux-gnu-ubuntu-18.04 clang_9.0.0
sudo mv clang_9.0.0 /usr/local
sudo mv clang_9.0.0/ /usr/local/
sudo rm -rf /usr/local/clang_9.0.0/
sudo mv clang_9.0.0/ /usr/local/
exit
sudo snap install ccls --classic
sudo apt install cmake
sudo apt install cmake-doc ninja-build
df
cmake --version
exit
byobu
exit
byobu
sudo reboot 
byobu
exit
byobu
exit
byobu
emacs --with-profile doom &
~/doom-emacs/bin/doom upgrade
~/doom-emacs/bin/doom sync
~/doom-emacs/bin/doom clean
emacs --with-profile doom &
vim --version
vim
vimtutor 
byobu
exec pipenv shell
exit
byobu
firefox &
byobu
firefox &
~/doom-emacs/bin/doom upgrade
emacs --with-profile doom &
~/doom-emacs/bin/doom upgrade
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
pandoc --version
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
~/doom-emacs/bin/doom sync
emacs --with-profile doom &
cd Downloads/
ls
tar xzvf linux-pandoc_2_9_1_1.tar.gz
ls

curl -SL https://github.com/lierdakil/pandoc-crossref/releases/download/v0.3.6.1b/linux-pandoc_2_9_1_1.tar.gz | tar -xJC
curl -SL https://github.com/lierdakil/pandoc-crossref/releases/download/v0.3.6.1b/linux-pandoc_2_9_1_1.tar.gz | tar -xzvJC
curl -SL https://github.com/lierdakil/pandoc-crossref/releases/download/v0.3.6.1b/linux-pandoc_2_9_1_1.tar.gz | tar -xzvJC .
ls
man tar
tar xzvf linux-pandoc_2_9_1_1.tar.gz
sudo mv pandoc-crossref0.3.6.1b/ /usr/local
cd /usr/local/pandoc-crossref0.3.6.1b/
ls
exit
gtypist esp.typ
exit
exit
byobu
df
sudo apt install haskell
sudo apt-get install haskell-platform
cabal install hledger
sudo apt remove hledger
sudo apt autoremove 
hledger --version
cabal install hledger
cabal hled
cabal install hledger --global
cabal uninstall hledger
cd .cabal/
ls
cd packages/
ls
cd hackage.haskell.org/
ls
hledger --version
cd hledger
ls
cd 1.16.2/
ls
cd ..
ls
cd ..
ls
cd bin/
ls
hledger --version
ls hledger 
./hledger 
./hledger --version
ls
exit
cabal install hledger
cabal install pretty-show
emacs&
cabal install runhaskell
cabal install haskell
cabal update
exit
exit
$PATH
cd 
exit
byobu
exit
hledger --version
hledger --help
sudo apt install stack
curl -sSL https://get.haskellstack.org/ | sh
sta
stack --version
stack --help
man stack
exit
byobu
exit
byobu
display Screenshot\ from\ 2020-02-14\ 13-51-15.png 
byobu
sudo shutdown -P now
byobu
cd learning/PythonDataScienceHandbook/
conda activate biosig
jupyter lab &
sudo shutdown -P now
exec pipenv shell
exit
byobu
firefox &
emacs&
thunderbird &
sudo shutdown -P now
byobu
firefox &
emacs&
hledger --version
cd Downloads/
ls
tar xvf hledger-flow_Linux_x86_64_v0.13.0.0_ced0b70.tar.gz 
ls
cd hledger-flow_Linux_x86_64_v0.13.0.0_ced0b70/
ls
cat buildinfo.txt 
ls
hledger-flow
ls
./hledger-flow 
cd ..
cd mv hledger-flow_Linux_x86_64_v0.13.0.0_ced0b70/ hledger-flow
mv hledger-flow_Linux_x86_64_v0.13.0.0_ced0b70/ hledger-flow
ls
sudo mv hledger-flow /usr/local
exit
exit
byobu
emacs&
thunderbird &
zoom &
~/doom-emacs/bin/doom sync
emacs&
sudo apt-get update
sudo apt remove cmake
sudo snap install cmake
sudo snap install --classic cmake
cmake --version
emacs&
~/doom-emacs/bin/doom doctor
sudo snap install libvterm
sudo apt install libvterm
sudo apt-get install neovim
~/doom-emacs/bin/doom doctor
neotoppm 
sudo apt-get install neovim
sudo apt-get install python3-neovim
sudo apt autoremove 
nvim &
jobs
fg %4
emacs&
sudo apt-get install libvterm-dev
emacs&
sudo apt install libtool
~/doom-emacs/bin/doom info
emacs&
exit
exit
byobu
~/doom-emacs/bin/doom upgrade
~/doom-emacs/bin/doom doctor
emacs&
anki-woodrow.anki &
sudo shutdown -P now
byobu
ecit
exit
byobu
emacs&
anki-woodrow.anki &
thunderbird &
firefox &
thunderbird &
anki-woodrow.anki &
sudo shutdown -P now
byobu
sudo apt install php
mysql_secure_installation
sudo apt install php
sudo apt install php php-mysql 
mysql_secure_installation
sudo apt remove install mariadb-server mariadb-client
sudo apt remove mariadb-server mariadb-client
sudo apt remove --purge mariadb-server mariadb-client
sudo apt install mariadb-server mariadb-client
mysql_secure_installation
emacs&
sudo apt install mariadb-server mariadb-client
sudo apt remove --purge mariadb-server mariadb-client
sudo apt autoremove 
sudo apt install mariadb-server mariadb-client
mysql_secure_installation
exit
google-chrome&
exit
exit
byobu
emacs&
google-chrome&
git clone git@github.com:phpactor/phpactor
git clone https://github.com/phpactor/phpactor.git
git checkout develop
cd phpactor/
git checkout develop 
git status 
ls
composer install
sudo apt install composer 
composer install
composer global require     d11wtq/boris     phpunit/phpunit     techlivezheng/phpctags
cd /usr/local/bin
sudo ln -s ~/your/projects/phpactor/bin/phpactor phpactor
ls
cd
cd phpactor/
cd /usr/local/bin/
ls
rm phpactor 
sudo rm phpactor 
ls
sudo ln -s ~/phpactor/bin/phpactor phpactor
ls
phpactor --version
cd
phpactor
phpactor --help
~/doom-emacs/bin/doom sync
emacs
emacs&
prettier --version
emacs&
source .bashrc 
emacs&
ls .composer
composer global require     d11wtq/boris     phpunit/phpunit     techlivezheng/phpctags
which composer
cd ${which composer}
which composer
cd /usr/bin/composer
composer --version
which composer
cd /usr/bin/
ls
ls composer
sudo apt install curl php-cli php-mbstring git unzip
sudo apt remove composer
cd
curl -sS https://getcomposer.org/installer -o composer-setup.php
$HASH = e0012edf3e80b6978849f5eff0d4b4e4c79ff1609dd1e613307e16318854d24ae64f26d17af3ef0bf7cfb710ca74755a
$HASH=e0012edf3e80b6978849f5eff0d4b4e4c79ff1609dd1e613307e16318854d24ae64f26d17af3ef0bf7cfb710ca74755a
$ HASH=e0012edf3e80b6978849f5eff0d4b4e4c79ff1609dd1e613307e16318854d24ae64f26d17af3ef0bf7cfb710ca74755a
HASH=e0012edf3e80b6978849f5eff0d4b4e4c79ff1609dd1e613307e16318854d24ae64f26d17af3ef0bf7cfb710ca74755a
php -r "if (hash_file('SHA384', 'composer-setup.php') === '$HASH') { echo 'Installer verified'; } else { echo 'Installer corrupt'; unlink('composer-setup.php'); } echo PHP_EOL;"
sudo php composer-setup.php --install-dir=/usr/local/bin --filename=composer
composer
composer --version
cd phpactor/
composer install
cd
ls .composer/
composer global require     d11wtq/boris     phpunit/phpunit     techlivezheng/phpctags
sudo composer global require     d11wtq/boris     phpunit/phpunit     techlivezheng/phpctags
composer fund
source .bashrc 
export PATH="~/.composer/vendor/bin:$PATH"
ls .composer/vendor/bin/
php-parse --version
php-parse --help
exit
exit
byobu
curl icanhazip.com
ssh cperezm@94.134.84.253
ssh-keygen
ssh cperezm@
exit
sudo shutdown -P now
byobu

#!/bin/bash

VUNDLE=~/.vim/bundle/Vundle.vim/ 

if [ -d ${VUNDLE} ]; then
  echo "Refreshing Vundle ..."
  cd ${VUNDLE} && git pull
else
  echo "Installing Vundle ..."
  git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
fi

echo "Updating plugins ..."
vim +PluginInstall +qall

echo "Cleaning up plugins ..."
vim +PluginClean +qall

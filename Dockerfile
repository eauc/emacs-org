FROM jare/emacs:latest as emacs
RUN apt-get update && apt-get install -y wget
RUN mkdir -p /home/emacs && \
    cd /home/emacs && \
    wget https://orgmode.org/org-9.1.9.tar.gz && \
    tar xvzf org-9.1.9.tar.gz
RUN git clone --recurse --branch test https://github.com/eauc/dotfiles /home/emacs/dotfiles
RUN mkdir -p /home/emacs/elisp
COPY ./elisp/install.el /home/emacs/elisp/install.el
RUN HOME=/home/emacs emacs --batch -l "/home/emacs/elisp/install.el"
COPY ./elisp/* /home/emacs/elisp/

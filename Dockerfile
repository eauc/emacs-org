FROM iquiw/alpine-emacs as emacs
RUN apk update && apk add git graphviz
COPY elisp/install.el /root/.emacs.d/
RUN emacs --batch -q -l "/root/.emacs.d/install.el"
COPY ./elisp/* /root/.emacs.d/
RUN git clone https://github.com/fniessen/org-html-themes.git
COPY ./css/theme.css /root/

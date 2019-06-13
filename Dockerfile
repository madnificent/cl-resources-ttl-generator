FROM semtech/mu-cl-resources

COPY . /app/dependencies/resources-ttl-generator/
ADD ./startup.lisp /usr/src/startup.lisp
ENV SUPPORTED_SCHEMES http,https

CMD sh /load-config.sh; sh mkdir /config/output; sbcl --load /usr/src/startup.lisp

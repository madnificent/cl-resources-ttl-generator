FROM semtech/mu-cl-resources:1.27.2

COPY . /app/dependencies/resources-ttl-generator/
COPY ./startup.lisp /usr/src/startup.lisp
ENV SUPPORTED_SCHEMES http,https

COPY ./start-generator.sh /start-generator.sh

CMD ["/start-generator.sh"]

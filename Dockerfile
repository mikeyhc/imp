FROM erlang:alpine

COPY _build/default/rel/imp /imp
RUN apk add --no-cache git docker
ENTRYPOINT ["/imp/bin/imp"]
CMD ["foreground"]

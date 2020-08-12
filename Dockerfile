FROM erlang:alpine

COPY _build/default/rel/imp /imp
ENTRYPOINT ["/imp/bin/imp"]
CMD ["foreground"]

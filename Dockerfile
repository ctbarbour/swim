FROM voidlock/erlang:latest

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

ONBUILD COPY . /usr/src/app
ONBUILD RUN rebar3 update

RUN apt-get update -qq -y && \
    apt-get install -qq -y supervisor sudo && \
    apt-get clean

EXPOSE 5888/udp

VOLUME /root/.cache
VOLUME /usr/src/app/_build

CMD ["/usr/bin/supervisord", "-c", "/etc/supervisor/conf.d/supervisord-swim.conf"]
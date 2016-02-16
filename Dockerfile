FROM voidlock/erlang:latest

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app

ONBUILD COPY . /usr/src/app
ONBUILD RUN rebar3 update

RUN apt-get update -qq -y && \
    apt-get install -qq -y sudo && \
    apt-get clean

EXPOSE 5000/udp

VOLUME /root/.cache
VOLUME /usr/src/app/_build

CMD ["/usr/src/app/rebar3"]
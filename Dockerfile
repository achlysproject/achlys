FROM erlang:alpine

RUN apk add make git

RUN mkdir -p /buildroot/achlys
WORKDIR /buildroot/achlys

COPY src           .
COPY include       .
COPY config        .
COPY tools         .
COPY Makefile      .
COPY rebar.config* .
COPY VERSION       .

RUN make release v=1

FROM alpine

RUN apk add openssl ncurses-libs

RUN VERSION=$(cat VERSION) && export achlys_VERSION=$VERSION
COPY --from=0 achlys-$achlys_VERSION /achlys

WORKDIR /
RUN rm -rf /buildroot

CMD ["/achlys/bin/achlys", "foreground"]

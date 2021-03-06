FROM node:14-alpine

RUN npm install -g uglify-js \
    && mkdir /app \
    && apk update \
    && apk add curl \
    && curl -L -o elm.gz https://github.com/elm/compiler/releases/download/0.19.1/binary-for-linux-64-bit.gz \
    && gunzip elm.gz \
    && chmod +x elm \
    && mv elm /usr/local/bin/ \
    && yarn global add create-elm-app


COPY . /app

RUN cd /app \
    && elm-app build
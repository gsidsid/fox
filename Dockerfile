FROM akabe/ocaml:ubuntu16.04_ocaml4.07.0
ENV TARGET homework1.ml

ENV PATH $PATH:/home/opam/.local/bin

COPY ./files .

RUN sudo apt-get update && \
    sudo apt-get upgrade -y && \
    sudo apt-get install -y zlib1g libgmp10 libzmq5 python3 python3-pip

RUN pip3 install --upgrade pip

RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get install -y git

RUN pip3 install bs4 requests termcolor

ADD . .

RUN ls

RUN echo "Analyzing file -> ${TARGET}"

CMD ./focstest.py -v ${TARGET}

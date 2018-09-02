FROM r-base
RUN apt update && apt install -y libcairo2-dev
WORKDIR /code
ADD . /code
RUN ./install.R
CMD ./main.R

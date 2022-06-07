FROM hseeberger/scala-sbt:17.0.2_1.6.2_2.13.8

WORKDIR /build

RUN apt-get update && apt-get install -y git \
    ca-certificates \
    curl \
    gnupg \
    lsb-release

RUN mkdir -p /etc/apt/keyrings

RUN curl -fsSL https://download.docker.com/linux/debian/gpg | gpg --dearmor -o /etc/apt/keyrings/docker.gpg

# https://docs.docker.com/engine/install/debian/
RUN echo \
      "deb [arch=$(dpkg --print-architecture) signed-by=/etc/apt/keyrings/docker.gpg] https://download.docker.com/linux/debian \
      $(lsb_release -cs) stable" | tee /etc/apt/sources.list.d/docker.list > /dev/null

RUN cat /etc/apt/sources.list.d/docker.list

RUN apt-get update

RUN apt-get install -y docker-ce docker-ce-cli containerd.io docker-compose-plugin runit-systemd

RUN git clone --depth 1 https://github.com/bitcoin-s/bitcoin-s.git

WORKDIR "/build/bitcoin-s"

ENTRYPOINT ["sbt", "appServer/docker:publish"]
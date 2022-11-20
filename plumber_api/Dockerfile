FROM rocker/r-ver:4.2.2

MAINTAINER Pedro Rafael D. Marinho (pedro.rafael.marinho@gmail.com)

RUN apt-get update -qq && apt-get install -y \
	libssl-dev \
	libcurl4-gnutls-dev \
	libxml2-dev

RUN R -e "install.packages('broom')"
RUN R -e "install.packages('plumber')"

COPY api_plumber.R api_plumber.R
COPY dados_joao_pessoa_com_coord.RData dados_joao_pessoa_com_coord.RData
COPY funcoes_para_plumber_joao_pessoa.R funcoes_para_plumber_joao_pessoa.R

# Run
EXPOSE 8080

CMD ["Rscript", "-e", "pr <- plumber::plumb('api_plumber.R'); pr$run(host='0.0.0.0', port=8080)"]
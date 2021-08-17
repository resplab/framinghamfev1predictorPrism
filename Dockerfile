FROM opencpu/base
RUN R -e 'remotes::install_github("resplab/framinghamfev1predictor")'
RUN R -e 'remotes::install_github("resplab/framinghamfev1predictorPrism")'
RUN echo "opencpu:opencpu" | chpasswd

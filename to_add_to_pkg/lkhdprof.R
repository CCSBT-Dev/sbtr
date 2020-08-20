library(PBSmodelling)

source('esc_rcode/get.all.data.r')
data <- get.all.files("basehupsqrt")

source("esc_rcode/TableLikelihoodComponents.r")
nll.table  <- likelihood.table(data.objects = data)

source("esc_rcode/PlotNLLComponents.r")
plot.NLL.by.steepness(nll.table,caption="basehupsqrt")



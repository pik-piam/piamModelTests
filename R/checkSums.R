checkSums <- function(x, cfg) {
  sgroup <- extractVariableGroups(levels(x$variable),keepOrigNames = TRUE)
  sgroup <- sgroup[names(sgroup) %in% levels(x$variable)]
  failed <- NULL

  for (i in 1:length(sgroup)) {
    if (any(abs(filter(x,variable%in%sgroup[[i]]) %>%
            group_by(region,period) %>%
            summarise(grsum=sum(value),.groups = "drop") %>%
            select("grsum")-
            filter(arrange(x,region,period),variable==names(sgroup[i]))[["value"]])

            >

            0.00001 )

        ) failed <- c(failed,names(sgroup[i]))
  }
  return(list(message="%# sums do not add up",
              failed=failed))
}

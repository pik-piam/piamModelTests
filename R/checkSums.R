#' @importFrom mip extractVariableGroups
#' @importFrom dplyr %>% filter group_by summarise select arrange

checkSums <- function(x) {
  sgroup <- extractVariableGroups(levels(x$variable),keepOrigNames = TRUE)
  sgroup <- sgroup[names(sgroup) %in% unique(x$variable)]
  failed <- NULL

  if (length(sgroup) > 0) for (i in 1:length(sgroup)) {
    if (!any(sgroup[[i]]%in%unique(x$variable))) {
      failed <- c(failed,names(sgroup[i]))
    } else {
      if (any(abs(filter(x,variable%in%sgroup[[i]]) %>%
                  group_by(region,period) %>%
                  summarise(grsum=sum(value),.groups = "drop") %>%
                  select("grsum")-
                  filter(arrange(x,region,period),variable==names(sgroup[i]))[["value"]])

              >

              0.00001 )

      ) failed <- c(failed,names(sgroup[i]))

      }
  }
  return(list(message="%# sums do not add up or are missing in the data or the cfg (shown here are the group totals)",
              failed=failed))
}

#-----------

library(devtools)
serve.install_github("rstudio/blogdown")
library(blogdown)
install_hugo()

#-----------

new_site(theme="calintat/minimal")
new_site(theme="dim0627/hugo_theme_robust")
new_site(theme="ageekymonk/hugo-tracks-theme")
new_site(theme="gyorb/hugo-dusk")
new_site(theme="zhe/hugo-theme-slim")
build_site()

#----------

 #'hugo'
 #'public'
 #'HUGO_VERSION'
 #'0.26'


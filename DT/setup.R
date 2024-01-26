library(kableExtra)
# knitr::knit_hooks$restore() # Pour les réinitialiser

is_html <- knitr::is_html_output()
is_latex <- knitr::is_latex_output()

fig.ext <- ifelse(is_latex,"pdf",ifelse(is_html,"svg", "jpg"))
fig.ext_cube <- ifelse(is_latex,"jpg","jpg")
dev <- ifelse(is_latex,"pdf","svg")
# dev2 <- knitr::opts_chunk$get("dev")
fig.path <- ifelse(is_latex,"pdf",ifelse(is_html,"html", "autres"))
knitr::opts_chunk$set(echo = FALSE,
                      fig.path = sprintf("img/bookdown/%s/", fig.path),
                      fig.ext = fig.ext,
                      dev = dev, dpi = 76,
                      cache = FALSE,
                      warning = FALSE,
                      message = FALSE,
                      fig.align = 'center',
                      fig.pos = "",
                      out.extra = NULL)
height_cube <- 1.7



# Fontawesome
htmltools::tagList(rmarkdown::html_dependency_font_awesome())

if(is_html){
    fa_arrow_circle_right <- '<i class="fas fa-arrow-circle-right"></i>'
    fa_r_project <- '<i class="fab fa-r-project"></i>'
}else {
    if(is_latex){
        fa_arrow_circle_right <- "\\faIcon{arrow-circle-right}"
        fa_r_project <- "\\faIcon{r-project}"
    }else {
        fa_arrow_circle_right <- "->"
        fa_r_project <- 'R'
    }
}

################################
########## KNITR ###############
################################
library(htmltools)
markdown_latex <- function(text, to = "latex"){
    res <- sapply(text, function(x){
        dir = tempdir()
        infile <- tempfile(fileext=".md", tmpdir = dir)
        # writeLines(x, infile)
        writeLines(enc2utf8(x), infile,
                   useBytes=TRUE)
        file.copy("biblio.bib",file.path(dir,"biblio.bib"))
        outfile <- tempfile(fileext=".tex", tmpdir = dir)
        rmarkdown::pandoc_convert(infile,
                                  to = to,
                                  from = "markdown",
                                  citeproc = FALSE,
                                  options = c("--bibliography=biblio.bib",
                                              "--biblatex"
                                              ),
                                  output = outfile,
                                  wd = dir)
        paste(readLines(outfile, encoding="UTF-8"), collapse = " ")
    })
    names(res) <- NULL
    res
}
converttext <- ifelse(is_latex,markdown_latex, \(x) x)
latex_emph <- function(entete = "Note", x, sep = "\n\n"){
    txt <- paste(sprintf("\\emph{%s}", x), collapse = sep)
    if(length(x)>1){
        entete <- paste0(entete,"s")
    }
    sprintf("\n\\emph{%s} : %s", entete, txt)
}
md_framed <- function(x, options){
    if (!is_latex || is.null(options$mdframed) || !options$mdframed || 
        is.null(options$fig.cap))
        return (x)
    # cap <- options$fig.cap
    # label <- fig$label
    # \label{fig:filtersdafcoefs}
    # \includegraphics[width=3cm,height=3cm]{example-image-a}
    #     \captionof{figure}{exampleimg}
    x <- gsub("\\\\begin\\{figure\\}(\\[!?\\w*\\])?",
              "", x)
    x <- gsub("\\caption","\\captiontmp", x, fixed = TRUE)
    # x <- gsub("^(\\n)*", "", x)
    x <- gsub("\n\n{\\centering",
              "\\begin{center}", x, fixed = TRUE)
    x <- sub("} \n\n}","}", x, fixed = TRUE)
    x <- gsub("\\end{figure}",
              "\\end{center}", x, fixed = TRUE)
    x
}

add_fig_opt_html <- function(x, options, nom_opt){
    nom_opt_complet <- sprintf("fig.%s", tolower(nom_opt))
    opt <- options[[nom_opt_complet]]
    if(!is.null(opt)){
        first_txt <- tags$p(opt[1], id = nom_opt, class = "title caption")
        all_txt <- c(list(first_txt),
                     lapply(opt[-1], tags$p, class = "caption"))
        txt <- withTags(
            div(class = "caption", id = nom_opt,
                tagList(all_txt)
            )
        )
        x <- paste(x,
                   as.character(txt),
                   sep = "\n")
    }
    x
}

add_fig_opt_latex <- function(x, options, nom_opt, 
                              prefix = nom_opt,
                              sep_multi = "---", sep_fichier = "\n\n"){
    if(is.null(prefix)){
        prefix <- nom_opt
    }
    if(is.null(sep_multi)){
        sep_multi <- "---"
    }
    if(is.null(sep_fichier)){
        sep_fichier <- "\n\n"
    }
    nom_opt_complet <- sprintf("fig.%s", tolower(nom_opt))
    opt <- options[[nom_opt_complet]]
    if(!is.null(opt)){
        x <- paste(x,
                   latex_emph(prefix,
                              markdown_latex(opt),
                              sep = sep_multi),
                   sep = sep_fichier)
    }
    x
}
cite_ref_latex <- function(x){
    split <- strsplit(x, " ")
    sapply(split, function(phrase){
        id_ref <- grep("@", phrase)
        for(i in id_ref){
            x <- phrase[i]
            suffix = gsub("^@\\w*","",x)
            toremoved <- paste0("@",suffix)
            nomref <- x
            for(i_c in seq_len(nchar(toremoved))){
                nomref <- gsub(substr(toremoved,i_c,i_c),"",
                               nomref, fixed = TRUE) 
            }
            phrase[i] <- sprintf("\\textcite{%s}%s",nomref, suffix)
        }
        paste(phrase, collapse = " ")
    })
}
add_footnote_latex <- function(x, options, envir = "figure", remove_envir = TRUE,
                               stop_centering = FALSE){
    
    fin_envir <- sprintf("\\end{%s}", envir)
    if(length(grep(fin_envir, x, fixed = TRUE)) == 0){
        return(x)
    }
    
    params <- list(Source = list(),
                   Champ = list(),
                   Note = list(sep_multi = "\n\n"),
                   Lecture = list(prefix = "Note de lecture"))
    ajouts <- "\\footnotesize"
    for (opt in names(params)){
        ajouts <- add_fig_opt_latex(ajouts, options, opt,
                                    prefix = params[[opt]]$prefix,
                                    sep_multi = params[[opt]]$sep_multi,
                                    sep_fichier = params[[opt]]$sep_fichier)
    }
    if(stop_centering){
        x <- sub("\\\\centering", "{\n\\\\centering", x)
        x <- paste0(x, "}\n")
    }
    x <- paste0(x, ajouts, "\n\\normalsize")
    if(remove_envir){
        x <- sub(fin_envir, "", x, fixed = TRUE)
        
        x <- paste0(x, fin_envir)  
    }
    x  
}

add_footnote_html <- function(x, options){
    for (opt in c("source", "champ", "note", "lecture")){
        x <- add_fig_opt_html(x, options, opt)
    }
    x
}
add_footnote_perso <- function(x, options, envir = "figure", remove_envir = TRUE,
                               stop_centering = FALSE){
    if(is_latex){
        res <-  add_footnote_latex(x, options, envir = envir,
                                   remove_envir = remove_envir,
                                   stop_centering = stop_centering)
        
    }else if(is_html){
        res <-  add_footnote_html(x, options)
    }else{
        res <-  x
    }
    res
}
add_footnote_kable <- function(x, options = knitr::opts_current$get(),
                               stop_centering = FALSE){
    envir = remove_envir = NULL
    if(length(grep("\\end{table}", x, fixed = TRUE)) >0){
        envir = "table"
        remove_envir = TRUE
    } else if(length(grep("\\end{longtable}", x, fixed = TRUE)) >0){
        envir = "longtable"
        remove_envir = FALSE
    } 
    x <- add_footnote_perso(x, options = options, envir = envir,
                            remove_envir = remove_envir,
                            stop_centering = stop_centering)
    
    class(x) <- "knitr_kable"
    if(is_latex){
        attr(x, "format") <- "latex"
    }else if(is_html){
        attr(x, "format") <- "html"
    }
    x
}

local({
    # hook_plot2 <- knitr::knit_hooks$get('plot')
    knitr::knit_hooks$set(plot = function(x, options) {
        if(is.null(options$fig.cap))
            return(knitr::hook_plot_md(x, options))
        if(is_latex){
            # res <-  knitr:::hook_plot_tex(x, options)
            options$fig.cap <- markdown_latex(options$fig.cap)
            res <-  knitr:::hook_plot_tex(x, options)
            # res <- md_framed(res, options)
            # res <- add_footnote_latex(res, options)
        }else if(is_html){
            res <- knitr::hook_plot_md(x, options)
            # res <- add_footnote_html(res, options)
        }else{
            res <-  knitr::hook_plot_md(x, options)
        }
        res <- add_footnote_perso(res, options)
        md_framed(res, options)
    })
})
# library(knitr)
# local({
#   hook_plot <- function (x, options) {
#       
#     if (options$fig.show == "animate") 
#         return(hook_plot_html(x, options))
#     base = knitr:::`%n%`(opts_knit$get("base.url"), "")
#     cap = knitr:::.img.cap(options)
#     alt = knitr:::.img.cap(options, alt = TRUE)
#     w = options[["out.width"]]
#     h = options[["out.height"]]
#     s = options$out.extra
#     a = options$fig.align
#     ai = options$fig.show == "asis"
#     lnk = options$fig.link
#     pandoc_html = cap != "" && is_html_output()
#     in_bookdown = isTRUE(opts_knit$get("bookdown.internal.label"))
#     plot1 = ai || options$fig.cur <= 1L
#     plot2 = ai || options$fig.cur == options$fig.num
#     to = pandoc_to()
#     from = pandoc_from()
#     if (is.null(w) && is.null(h) && is.null(s) && is.null(options$fig.alt) && 
#         a == "default" && !(pandoc_html && in_bookdown)) {
#         nocap = cap == "" && !is.null(to) && !grepl("^markdown", 
#             to) && (options$fig.num == 1 || ai) && !grepl("-implicit_figures", 
#             from)
#         res = sprintf("![%s](%s%s)", cap, base, .upload.url(x))
#         if (!is.null(lnk) && !is.na(lnk)) 
#             res = sprintf("[%s](%s)", res, lnk)
#         res = paste0(res, if (nocap) 
#             "<!-- -->"
#         else "", if (is_latex_output()) 
#             " "
#         else "")
#         return(res)
#     }
#     add_link = function(x) {
#         if (is.null(lnk) || is.na(lnk)) 
#             return(x)
#         sprintf("<a href=\"%s\" target=\"_blank\">%s</a>", lnk, 
#             x)
#     }
#     if (pandoc_html && !isTRUE(grepl("-implicit_figures", from))) {
#         d1 = if (plot1) 
#             sprintf("<div class=\"figure\"%s>\n", knitr:::css_text_align(a))
#         d2 = sprintf("<p class=\"caption\">Note: %s</p>", cap)
#         if(!is.null(options$comment)){
#           d2 <- sprintf("<p class='comment'>%s</p>%s",options$comment, d2)
#         }
#         img = sprintf("<img src=\"%s\" alt=\"%s\" %s />", paste0(opts_knit$get("base.url"), 
#             knitr:::.upload.url(x)), alt, knitr:::.img.attr(w, h, s))
#         img = add_link(img)
#         if (isTRUE(options$fig.topcaption)) {
#             paste0(d1, if (ai || options$fig.cur <= 1) 
#                 d2, img, if (plot2) 
#                 "</div>") 
#         }
#         else {
#             paste0(d1, img, if (plot2) 
#                 paste0("\n", d2, "\n</div>"))
#         }
#     }
#     else add_link(.img.tag(.upload.url(x), w, h, alt, c(s, sprintf("style=\"%s\"", 
#         css_align(a)))))
# }
#   knitr::knit_hooks$set(plot = function(x, options) {
#       hook_plot(x, options)
#   })
# })


## Graphiques 3D

library(plot3D)
global_plot <- function(data, q, method, degree, phi = 40,
                        theta = 40,
                        titre = NULL){
    
    data_tri <- data[(data$q %in% as.numeric(q)) &
                         (data$method %in% method) &
                         (data$degree %in% as.numeric(degree)),]
    scatter_3D(data_tri, titre = titre, phi = phi, theta = theta)
}
scatter_3D <- function(x, titre = NULL, phi = 40,
                       theta = 40){
    add <- nrow(x) >0
    if(add){
        with(x, 
             scatter3D(x = fidelity.weight,
                       y = smoothness.weight,
                       z = timeliness.weight,
                       colvar = NULL, 
                       phi = phi, theta = theta,
                       # bty = "g",
                       pch = 1,
                       # cex = 0.1, alpha = 0.4,
                       ticktype = "detailed",
                       xlim = c(0,1),
                       ylim = c(0,1),
                       zlim = c(0,1), 
                       xlab = "\n\nFidelity",
                       ylab ="\n\nSmoothness",
                       zlab = "\n\nTimeliness",
                       main = titre))
        polygon3D(x = c(0,0,1), y = c(0,1,0), z = c(1,0,0),
                  add = add, alpha = 0.2,
                  ticktype = "detailed",
                  phi = phi, theta = theta,
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }else{
        scatter3D(x = -2,
                  y = 2,
                  z = 2,
                  colvar = NULL, 
                  phi = phi, theta = theta,
                  # bty = "g",
                  pch = 1,
                  # cex = 0.1, alpha = 0.4,
                  ticktype = "detailed",
                  xlim = c(0,1),
                  ylim = c(0,1),
                  zlim = c(0,1), 
                  xlab = "\n\nFidelity",
                  ylab ="\n\nSmoothness",
                  zlab = "\n\nTimeliness",
                  main = titre)
    }
}


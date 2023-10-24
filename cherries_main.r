## Main script for cherries

source('cherries.r')
source('cherry_compatibility.r')
source('cherry_compatibility.r')
source('cherries_rosbreed.r')


## Quarto -----
pacman::p_load(quarto)
## quarto_render('cherries_website/index.qmd')
## ?quarto_render
getwd()
quarto_render('cherries_website')
getwd()
## quarto_render('test_presentation.qmd')






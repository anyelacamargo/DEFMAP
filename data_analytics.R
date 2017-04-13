library(rgdal);
library('ggplot2');
library('dismo')
library('rgdal')
library('XML')
library('foreign')
library('sp');
library('dplyr')
library('tmap')


plotMapGrid = function(f, metadata, probl_list, varnames_list) 
{
  
  for(cname in varnames_list)
  {
    cl = c(quantile(f[[cname]], probl_list)[[1]], quantile(f[[cname]], probl_list)[[2]], 
           quantile(f[[cname]], probl_list)[[3]], quantile(f[[cname]], probl_list)[[4]]);
    i = which(metadata$Code == cname);
    j = which(gadm[[cname]] == max(gadm[[cname]]));
    ptitle = paste('Highest yield coord: ', gadm[['E']][j], ',', gadm[['N']][j], sep='')
    crop = as.character(metatable$Description[i]);
    
    print(
      tm_shape(f) + 
        tm_fill(cname, style="fixed", breaks = cl,  palette="Greens",
                title=crop) + 
        tm_borders("white") +
        tm_legend(title = ptitle, outside = TRUE, title.size = 1.3, text.size = .8,
                  title.position = c("right", "bottom")) +
        tm_layout(frame = FALSE));
  }
  
}

gadm = readOGR(dsn = ".", layer = "Grid_5km_ENG_ONLY_with_PUBLIC_USE_dataset")
metatable = read.table('crop_metadata.csv', sep=',', header=T);
h = data.frame(gadm@data);
probl_list = c(30, 50, 97, 98)/100;
pdf('england_crop_map.pdf');
plotMapGrid(gadm, metatable, probl_list, colnames(gadm@data)[7:55]);
dev.off();



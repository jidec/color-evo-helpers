vizID <- function(id,repo_path="E:/dragonfly-patterner",
                        viz_image=TRUE, img_suffix_ext =".jpg",
                        viz_segment=F, seg_suffix_ext ="_fore_segment.png",
                        viz_pattern_folder=NULL, pat_suffix_ext ="_fore_pattern.png"){
    library(imager)
    if(viz_image){
        path <- paste0(repo_path,'/data/all_images/',id,img_suffix_ext)
        img <- load.image(path)
        plot(img)
    }
    if(viz_segment){
        path <- paste0(repo_path,'/data/segments/',id,seg_suffix_ext)
        img <- load.image(path)
        plot(img)
    }
    if(!is.null(viz_pattern_folder)){
        path <- paste0(repo_path,'/data/patterns/',viz_pattern_folder,'/',id,pat_suffix_ext)
        img <- load.image(path)
        plot(img)
    }
}

#visualizeID("INAT-49781305-4",repo_path = "E:/dragonfly-patterner")
#visualizeID(id="WRK-WS-02580",repo_path="D:/wing-color/",viz_image = F,viz_segment = T, viz_pattern_folder = "grouped")


gid2date <- function(x) {
    
    chr <- gsub("_[a-z]+_[a-z]+_[0-9]$", "", gsub("^gid_", "", x))
    
    as.Date(chr, format = "%Y_%m_%d")
}

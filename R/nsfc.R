#' get nsfc items
#' @description get nsfc items from http://www.sciencenet.cn/, by key
#' @param url url
#' @param header headers 
#' @param subject subject, ex:H0801
#' @param search string to search, ex: "m6A"
#' @param yearStart number, year of start
#' @param yearEnd number, year of start
#' @param itemCategory category
#' @param fundStart fund min
#' @param fundEnd fund max
#' @param abstract a logical argument, default is FLASE
#' @return dataframe
#' @export
#'
#' @examples nsfc(yearStart=2018)
nsfc <- function(url,header,subject,search,yearStart,yearEnd,itemCategory,fundStart,fundEnd,abstract=FALSE){
    if (!is.logical(abstract)) stop(tmcn::toUTF8('abstract\u5E94\u8BE5\u662F\u903B\u8F91\u53D8\u91CF'))
    library(httr)
    library(rvest)
    library(magrittr)
    #bulid url
    if (missing(url)){
        #build url
        url="http://fund.sciencenet.cn/search?"
        if (!missing(search))       url=paste0(url,"name=",search)
        if (!missing(yearStart))    url=paste0(url,"&yearStart=",yearStart)
        if (!missing(yearEnd))      url=paste0(url,"&yearEnd=",yearEnd)
        url = paste0(url,'&keyWord=1') #using key word query
        if (!missing(subject))      url=paste0(url,"&subject=",subject)
        if (!missing(itemCategory)) url=paste0(url,"&category",itemCategory)
        if (!missing(fundStart))    url=paste0(url,"&fundStart",fundStart)
        if (!missing(fundEnd))      url=paste0(url,"&fundEnd",fundEnd)
        url=paste0(url,"&submit=list")
        #get total page number
        if (missing(header)) r <- GET(url)
        if (!missing(header)) r <- GET(url,add_headers(.headers = header))
        if (status_code(r) != 200) stop(tmcn::toUTF8('\u67E5\u8BE2\u7F51\u9875\u51FA\u9519,\u8BF7\u91CD\u65B0\u8D4B\u503C\u5173\u952E\u5B57,\u6216\u7A0D\u540E\u518D\u6B21\u8FDB\u884C,\u6216\u8BBE\u7F6Eheader\u540E\u518D\u8FDB\u884C'))
        r_content=content(r)
        all_items = r_content %>% 
            html_nodes(xpath = '//*[@id="l"]/b[1]') %>%
            html_text(trim = TRUE)
        page_number = ceiling(as.numeric(all_items)/10)
        url=paste0(url,"&page=",1:page_number)
        cat(tmcn::toUTF8('\u5171\u6709'),all_items,
            tmcn::toUTF8('\u4E2A\u9879\u76EE'),'\n')
        cat(tmcn::toUTF8('\u5171\u6709'),page_number,
            tmcn::toUTF8('\u4E2A\u7F51\u9875'),'\n')
    }
    url_length=length(url)
    #sleep time (seconds)
    if (url_length == 1) sleep.time = 0
    if (url_length >1 & url_length <= 10) sleep.time = 15
    if (url_length >10 & url_length <= 20) sleep.time = 30
    if (url_length > 20) sleep.time = 60
    #wheter to continue relaying on time, if time needed is more than 2min, note
    time_message=paste0(tmcn::toUTF8('\u5927\u7EA6\u9700\u8981'),
                        url_length*sleep.time/60,
                        tmcn::toUTF8('\u5206\u949F'))
    cat(crayon::red$bgWhite(tmcn::toUTF8('\u6574\u7406\u6807\u9898')),'\n')
    if (url_length*sleep.time/60 <= 2){
        message(time_message,'\n')
    }else{
        message(time_message)
        message(tmcn::toUTF8('\u8BF7\u95EE\u4F60\u8FD8\u8981\u7EE7\u7EED\u5417?'))
        ask = c(tmcn::toUTF8('\u7EE7\u7EED'),tmcn::toUTF8('\u4E0D\u7EE7\u7EED'))
        res <- svDialogs::dlg_list(choices = ask,
                                   preselect=FALSE,
                                   multiple = FALSE)$res
        if (nchar(res)==3) {
            opt <- options(show.error.messages = FALSE)
            on.exit(options(opt))
            stop()
        }
    }
    # scrab
    prgbar<- txtProgressBar(min = 0, max = url_length,
                            style = 3,initial = 0,width = 25)
    for (i in 1:url_length) {
        if (i==1) df=data.frame()
        if (missing(header)) r <- GET(url[i])
        if (!missing(header)) r <- GET(url[i],add_headers(.headers = header))
        #check request successfully
        if (status_code(r) != 200) {
            return(df)
            stop(tmcn::toUTF8('\u83B7\u53D6\u7F51\u9875\u51FA\u9519,\u8BF7\u7A0D\u540E\u518D\u5C1D\u8BD5'))
        }
        r_content=content(r)
        #1. study_type
        study_type = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]//div/p[1]/i') %>%
            html_text(trim = TRUE) %>%
            gsub(pattern = "\u00A0",replacement = " ")
        #2. item
        item = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]//p/a') %>%
            html_text(trim = TRUE) %>%
            gsub(pattern = "\u00A0",replacement = " ")
        #3. person
        person = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]//div/p[1]/span[1]/i') %>%
            html_text(trim = TRUE) %>%
            gsub(pattern = "\u00A0",replacement = " ")
        #4. department
        department = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]//div/p[1]/span[2]/i') %>%
            html_text(trim = TRUE) %>%
            gsub(pattern = "\u00A0",replacement = " ")
        #5. id
        id = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]/div[position()<=10]/div/p[1]/b') %>%
            html_text(trim = TRUE) %>%
            gsub(pattern = "\u00A0",replacement = " ") %>%
            gsub(pattern = " ",replacement = "")
        #6. year
        year = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]//div/p[1]/span[3]/b') %>%
            html_text(trim = TRUE) %>%
            gsub(pattern = "\u00A0",replacement = " ")
        #7. fund
        fund = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]//div/p[2]/span[1]/b') %>%
            html_text(trim = TRUE) %>%
            gsub(pattern = "\u00A0",replacement = " ")
        #8. abstract url
        abstract_url = r_content %>% 
            html_nodes(xpath = '//*[@id="resultLst"]//p/a') %>%
            html_attr("href") %>%
            gsub(pattern = "\u00A0",replacement = " ")
        ###data.frame.i
        # if not missing subject, suject will be added to df
        if (!missing(subject)) {
            subject.text = r_content %>% 
                html_nodes(xpath = '//*[@id="tab1"]/form/div[2]/div[3]/span[2]/span[1]/span/span[1]') %>%
                html_text(trim = TRUE)
            df.i = data.frame(subject.text,study_type,item,person,department,
                              id,year,fund,abstract_url)
        }else{
            df.i = data.frame(study_type,item,person,department,
                              id,year,fund,abstract_url)
        }
        df = rbind(df,df.i)
        #generate char
        setTxtProgressBar(pb = prgbar, value = i)
        if (i != url_length) Sys.sleep(sleep.time)
    }
    close(prgbar)
    if (!missing(subject)){
        colnames(df)=c(tmcn::toUTF8('\u5B66\u79D1'),
                       tmcn::toUTF8('\u7814\u7A76\u7C7B\u578B'),
                       tmcn::toUTF8('\u9879\u76EE\u540D\u79F0'),
                       tmcn::toUTF8('\u8D1F\u8D23\u4EBA'),
                       tmcn::toUTF8('\u5355\u4F4D'),
                       tmcn::toUTF8('\u9879\u76EE\u7F16\u53F7'),
                       tmcn::toUTF8('\u6279\u51C6\u5E74\u5EA6'),
                       tmcn::toUTF8('\u91D1\u989D'),
                       tmcn::toUTF8('\u6458\u8981\u94FE\u63A5'))
    }else{
        colnames(df)=c(tmcn::toUTF8('\u7814\u7A76\u7C7B\u578B'),
                       tmcn::toUTF8('\u9879\u76EE\u540D\u79F0'),
                       tmcn::toUTF8('\u8D1F\u8D23\u4EBA'),
                       tmcn::toUTF8('\u5355\u4F4D'),
                       tmcn::toUTF8('\u9879\u76EE\u7F16\u53F7'),
                       tmcn::toUTF8('\u6279\u51C6\u5E74\u5EA6'),
                       tmcn::toUTF8('\u91D1\u989D'),
                       tmcn::toUTF8('\u6458\u8981\u94FE\u63A5')) 
    }
    if (!abstract) return(df)
    if (abstract){
        cat(crayon::red$bgWhite(tmcn::toUTF8('\u6574\u7406\u6458\u8981')),'\n')
        #all abstract urls
        abstract_url = as.character(df[,ncol(df)])
        #wheter to continue depends on time consuming
        time_consume = ifelse (length(abstract_url) <= 9, 15,30)
        message(tmcn::toUTF8('\u5927\u7EA6\u9700\u8981'),length(abstract_url)*time_consume/60,tmcn::toUTF8('\u5206\u949F'))
        message(tmcn::toUTF8('\u8BF7\u95EE\u4F60\u8FD8\u8981\u7EE7\u7EED\u5417?'))
        ask = c(tmcn::toUTF8('\u7EE7\u7EED'),tmcn::toUTF8('\u4E0D\u7EE7\u7EED'))
        res <- svDialogs::dlg_list(choices = ask,
                                   preselect=FALSE,
                                   multiple = FALSE)$res
        if (nchar(res)==3) {
            return(df)
            opt <- options(show.error.messages = FALSE)
            on.exit(options(opt))
            stop()
        }
        # if nchar(res) == 2, continue
        # scrab
        prgbar<- txtProgressBar(min = 0, max = length(abstract_url),
                                style = 3,initial = 0,width = 25)
        for (i in 1:length(abstract_url)) {
            if (i==1) df_abstract = data.frame()
            abs_url.i=abstract_url[i]
            if (missing(header)) r <- GET(abs_url.i)
            if (!missing(header)) r <- GET(abs_url.i,add_headers(.headers = header))
            #check request successfully
            if (status_code(r) != 200) {
                return(df)
                stop(tmcn::toUTF8('\u83B7\u53D6\u7F51\u9875\u51FA\u9519,\u8BF7\u7A0D\u540E\u518D\u5C1D\u8BD5'))
            }
            r_content=content(r)
            #1. subject_abs
            subject_abs = r_content %>%
                sub(pattern = tmcn::toUTF8(".*\u5B66\u79D1\u5206\u7C7B</th><td colspan"),replacement = '') %>%
                sub(pattern = tmcn::toUTF8('<th>\u9879\u76EE\u8D1F\u8D23\u4EBA.*'),replacement = "") %>%
                sub(pattern = '.*">',replacement = "") %>%
                sub(pattern = '</td>.*',replacement = "")%>%
                gsub(pattern = "\u00A0",replacement = " ")%>%
                gsub(pattern = " ",replacement = "")
            #2. time_limit
            time_limit = r_content %>%
                sub(pattern = tmcn::toUTF8('.*\u7814\u7A76\u671F\u9650</th><td'),replacement = '') %>%
                sub(pattern = tmcn::toUTF8('<th>\u4E2D\u6587\u4E3B\u9898\u8BCD.*'),replacement = "") %>%
                sub(pattern = '.*">',replacement = "") %>%
                sub(pattern = '</td>.*',replacement = "")%>%
                gsub(pattern = "<br[ /]{,2}>",replacement = "") %>%
                gsub(pattern = " ",replacement = "")
            #3. key_word_cn
            key_word_cn = r_content %>%
                sub(pattern = tmcn::toUTF8('.*<th>\u4E2D\u6587\u4E3B\u9898\u8BCD'),replacement = '') %>%
                sub(pattern = tmcn::toUTF8('<th>\u82F1\u6587\u4E3B\u9898\u8BCD.*') ,replacement = "") %>%
                sub(pattern = '.*">',replacement = "") %>%
                sub(pattern = '</td>.*',replacement = "")
            #4. key_word_english
            key_word_english = r_content %>%
                sub(pattern = tmcn::toUTF8('.*<th>\u82F1\u6587\u4E3B\u9898\u8BCD') ,replacement = "") %>%
                sub(pattern = '">',replacement = "erplacereplace") %>%
                sub(pattern = '.*erplacereplace',replacement = "") %>%
                sub(pattern = '</td>.*',replacement = "")
            #5. abs_cn
            abs_cn = r_content %>%
                sub(pattern = tmcn::toUTF8('.*<th>\u4E2D\u6587\u6458\u8981</th>'),replacement = '') %>%
                sub(pattern = '<td>',replacement = "erplacereplace") %>%
                sub(pattern = '.*erplacereplace',replacement = "") %>%
                sub(pattern = '</td>.*',replacement = "")
            #6. abs_english
            abs_english = r_content %>%
                sub(pattern = tmcn::toUTF8('.*<th>\u82F1\u6587\u6458\u8981</th>') ,replacement = '') %>%
                sub(pattern = '<td>',replacement = "erplacereplace") %>%
                sub(pattern = '.*erplacereplace',replacement = "") %>%
                sub(pattern = '</td>.*',replacement = "")
            df_abstract.i=data.frame(subject_abs,time_limit,
                                     key_word_cn,key_word_english,
                                     abs_cn,abs_english)
            df_abstract=rbind(df_abstract,df_abstract.i)
            if (i == length(abstract_url)){
                #rename
                colnames(df_abstract)=c(tmcn::toUTF8('\u5B66\u79D1\u5206\u7C7B_\u6765\u81EA\u6458\u8981'),
                                        tmcn::toUTF8('\u7814\u7A76\u671F\u9650'),
                                        tmcn::toUTF8('\u4E2D\u6587\u4E3B\u9898\u8BCD'),
                                        tmcn::toUTF8('\u82F1\u6587\u4E3B\u9898\u8BCD'),
                                        tmcn::toUTF8('\u4E2D\u6587\u6458\u8981'),
                                        tmcn::toUTF8('\u82F1\u6587\u6458\u8981')
                )
            }
            #generate char
            setTxtProgressBar(pb = prgbar, value = i)
            if (all((length(abstract_url)>2),
                    (i != length(abstract_url))))
                Sys.sleep(time_consume)
        }
        close(prgbar)
        df = cbind(df,df_abstract)
        return(df)
    }
}
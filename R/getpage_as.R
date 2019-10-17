
getpage_as <- function(subject,search,yearStart,yearEnd,
         itemCategory,fundStart,fundEnd){
library(httr)
library(rvest)
library(magrittr)
#build url
url="http://fund.sciencenet.cn/search?"
if (!missing(search))       url=paste0(url,"name=",do::inner_Add_Symbol(search))
if (!missing(yearStart))    url=paste0(url,"&yearStart=",yearStart)
if (!missing(yearEnd))      url=paste0(url,"&yearEnd=",yearEnd)
url = paste0(url,'&keyWord=1') #using key word query
if (!missing(subject))      url=paste0(url,"&subject=",subject)
if (!missing(itemCategory)){
    itemCategory<-get_category(itemCategory = itemCategory)
    url=paste0(url,"&category",itemCategory)
}
if (!missing(fundStart))    url=paste0(url,"&fundStart",fundStart)
if (!missing(fundEnd))      url=paste0(url,"&fundEnd",fundEnd)
url1=paste0(url,"&submit=list&order=funding&orderType=asc")
#get total page number
r <- GET(url1)
if (status_code(r)==429){
    head_response=headers(r)
    sleep_time=head_response$`retry-after`
    cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+20,tmcn::toUTF8('\u79D2'),'\n')
    a = Sys.time()
    cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
    cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
        as.character(a+as.numeric(sleep_time)+20),'\n')
    Sys.sleep(as.numeric(sleep_time)+20)
    if (missing(header)) r <- GET(url1)
    if (!missing(header)) r <- GET(url1,add_headers(.headers = header))
    if (status_code(r)==429){
        head_response=headers(r)
        sleep_time=head_response$`retry-after`
        cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+40,tmcn::toUTF8('\u79D2'),'\n')
        a = Sys.time()
        cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
        cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
            as.character(a+as.numeric(sleep_time)+20),'\n')
        Sys.sleep(as.numeric(sleep_time)+40)
        if (missing(header)) r <- GET(url1)
        if (!missing(header)) r <- GET(url1,add_headers(.headers = header))
    }
}else if (status_code(r) != 200){
    stop(status_code(r),tmcn::toUTF8(':\u67E5\u8BE2\u7F51\u9875\u51FA\u9519,\u8BF7\u91CD\u65B0\u8D4B\u503C\u5173\u952E\u5B57,\u6216\u7A0D\u540E\u518D\u6B21\u8FDB\u884C,\u6216\u8BBE\u7F6Eheader\u540E\u518D\u8FDB\u884C'))
}
r_content=content(r)
all_items = r_content %>%
    html_nodes(xpath = '//*[@id="l"]/b[1]') %>%
    html_text(trim = TRUE) %>%
    as.numeric()
page_number = ceiling(all_items/10)
fund.min=r_content %>%
    html_nodes(xpath = '//*[@id="resultLst"]/div[1]/div/p[2]/span[1]/b') %>%
    html_text(trim = TRUE)
url2=paste0(url,"&submit=list&order=funding&orderType=desc")
r <- GET(url2)
if (status_code(r)==429){
    head_response=headers(r)
    sleep_time=head_response$`retry-after`
    cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+20,tmcn::toUTF8('\u79D2'),'\n')
    a = Sys.time()
    cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
    cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
        as.character(a+as.numeric(sleep_time)+20),'\n')
    Sys.sleep(as.numeric(sleep_time)+20)
    if (missing(header)) r <- GET(url2)
    if (!missing(header)) r <- GET(url2,add_headers(.headers = header))
    if (status_code(r)==429){
        head_response=headers(r)
        sleep_time=head_response$`retry-after`
        cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+40,tmcn::toUTF8('\u79D2'),'\n')
        a = Sys.time()
        cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
        cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
            as.character(a+as.numeric(sleep_time)+20),'\n')
        Sys.sleep(as.numeric(sleep_time)+40)
        if (missing(header)) r <- GET(url2)
        if (!missing(header)) r <- GET(url2,add_headers(.headers = header))
    }
}else if (status_code(r) != 200){
    stop(status_code(r),tmcn::toUTF8(':\u67E5\u8BE2\u7F51\u9875\u51FA\u9519,\u8BF7\u91CD\u65B0\u8D4B\u503C\u5173\u952E\u5B57,\u6216\u7A0D\u540E\u518D\u6B21\u8FDB\u884C,\u6216\u8BBE\u7F6Eheader\u540E\u518D\u8FDB\u884C'))
}
r_content=content(r)
fund.max=r_content %>%
    html_nodes(xpath = '//*[@id="resultLst"]/div[1]/div/p[2]/span[1]/b') %>%
    html_text(trim = TRUE)
cat(tmcn::toUTF8('\u5171\u6709'),all_items,
    tmcn::toUTF8('\u4E2A\u9879\u76EE'),'\n')
cat(tmcn::toUTF8('\u5171\u6709'),page_number,
    tmcn::toUTF8('\u4E2A\u7F51\u9875'),'\n')
cat(tmcn::toUTF8('\u6700\u5C0F\u91D1\u989D'),fund.min,'\n')
cat(tmcn::toUTF8('\u6700\u5927\u91D1\u989D'),fund.max,'\n')
}

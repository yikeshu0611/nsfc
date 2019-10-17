#' get nsfc subject
#' @description get nsfc subject from http://www.sciencenet.cn/, by key
#'
#' @param url url
#' @param header header
#' @param subject subject, ex:H0801
#' @param yearStart number, year of start
#' @param year_ascend a logical argument, default is TRUE, ordre by year ascending
#' @param yearEnd number, year of start
#'
#' @return dataframe
#' @export
#'
#' @examples subject(yearStart=2018)
subject <- function(url,header,subject,yearStart,yearEnd,year_ascend=TRUE){
    library(httr)
    library(rvest)
    library(magrittr)
    #bulid url and get page_number
    if (missing(url)){
        #build url
        url="http://fund.sciencenet.cn/search/smallSubject?"
        if (!missing(subject))      url=paste0(url,"&subject=",subject)
        if (!missing(yearStart))    url=paste0(url,"&yearStart=",yearStart)
        if (!missing(yearEnd))      url=paste0(url,"&yearEnd=",yearEnd)
        if (year_ascend){
            url=paste0(url,"&submit=list&order=searchYearEnd&orderType=asc")
        }else{
            url=paste0(url,"&submit=list&order=searchYearEnd&orderType=desc")
        }
        #get total page number
        if (missing(header)) r <- GET(url)
        if (!missing(header)) r <- GET(url,add_headers(.headers = header))
        if (status_code(r)==429){
            head_response=headers(r)
            sleep_time=head_response$`retry-after`
            cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+20,tmcn::toUTF8('\u79D2'),'\n')
            a = Sys.time()
            cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
            cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                as.character(a+as.numeric(sleep_time)+20),'\n')
            Sys.sleep(as.numeric(sleep_time)+20)
            if (missing(header)) r <- GET(url)
            if (!missing(header)) r <- GET(url,add_headers(.headers = header))
            if (status_code(r)==429){
                head_response=headers(r)
                sleep_time=head_response$`retry-after`
                cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+40,tmcn::toUTF8('\u79D2'),'\n')
                a = Sys.time()
                cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
                cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                    as.character(a+as.numeric(sleep_time)+20),'\n')
                Sys.sleep(as.numeric(sleep_time)+40)
                if (missing(header)) r <- GET(url)
                if (!missing(header)) r <- GET(url,add_headers(.headers = header))
            }
        }else if (status_code(r) != 200)
            stop(status_code(r),tmcn::toUTF8(':\u67E5\u8BE2\u7F51\u9875\u51FA\u9519,\u8BF7\u91CD\u65B0\u8D4B\u503C\u5173\u952E\u5B57,\u6216\u7A0D\u540E\u518D\u6B21\u8FDB\u884C,\u6216\u8BBE\u7F6Eheader\u540E\u518D\u8FDB\u884C'))
        r_content=content(r)
        all_items = r_content %>%
            html_nodes(xpath = '//*[@id="l"]/b[1]') %>%
            html_text(trim = TRUE) %>%
            as.numeric()
        page_number = ceiling(all_items/10)
        url=paste0(url,"&page=",1:page_number)
        cat(tmcn::toUTF8('\u5171\u6709'),all_items,
            tmcn::toUTF8('\u4E2A\u9879\u76EE'),'\n')
        cat(tmcn::toUTF8('\u5171\u6709'),page_number,
            tmcn::toUTF8('\u4E2A\u7F51\u9875'),'\n')
        #url is not missing
    }else if (any(grepl('search',url))){ #theme page url
        #get total page number
        if (missing(header)) r <- GET(url)
        if (!missing(header)) r <- GET(url,add_headers(.headers = header))
        if (status_code(r)==429){
            head_response=headers(r)
            sleep_time=head_response$`retry-after`
            cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+20,tmcn::toUTF8('\u79D2'),'\n')
            a = Sys.time()
            cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
            cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                as.character(a+as.numeric(sleep_time)+20),'\n')
            Sys.sleep(as.numeric(sleep_time)+20)
            if (missing(header)) r <- GET(url)
            if (!missing(header)) r <- GET(url,add_headers(.headers = header))
            if (status_code(r)==429){
                head_response=headers(r)
                sleep_time=head_response$`retry-after`
                cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+40,tmcn::toUTF8('\u79D2'),'\n')
                a = Sys.time()
                cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
                cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                    as.character(a+as.numeric(sleep_time)+20),'\n')
                Sys.sleep(as.numeric(sleep_time)+40)
                if (missing(header)) r <- GET(url)
                if (!missing(header)) r <- GET(url,add_headers(.headers = header))
            }
        }else if (status_code(r) != 200)
            stop(status_code(r),tmcn::toUTF8(':\u67E5\u8BE2\u7F51\u9875\u51FA\u9519,\u8BF7\u91CD\u65B0\u8D4B\u503C\u5173\u952E\u5B57,\u6216\u7A0D\u540E\u518D\u6B21\u8FDB\u884C,\u6216\u8BBE\u7F6Eheader\u540E\u518D\u8FDB\u884C'))
        r_content=content(r)
        all_items = r_content %>%
            html_nodes(xpath = '//*[@id="l"]/b[1]') %>%
            html_text(trim = TRUE) %>%
            as.numeric()
        page_number = length(url)
    }else if(any(grepl('project',url))){ #abstract url
        page_number=length(url)
    }else{ #search and project represent 2 types of url
        stop(tmcn::toUTF8('\u8BF7\u68C0\u67E5\u7F51\u5740\u662F\u5426\u6B63\u786E'))
    }
    #if page_number is not zero, we will conduct
    if (page_number != 0){
        if (page_number >20){
            page_number=20
            cat(tmcn::toUTF8('\u4EC5\u4EC5\u6574\u7406\u524D20\u9875'),'\n')
        }
        # scrab
        cat(tmcn::toUTF8('\u9875\u7801:\u9879\u76EE\u5E8F\u53F7\n'))
        for (i in 1:page_number) {
            if (i==1) {
                df=data.frame()
                df_theme=data.frame()
                df_theme_nokey=data.frame()
            }
            # scrab theme part
            if (missing(header)) r <- GET(url[i])
            if (!missing(header)) r <- GET(url[i],add_headers(.headers = header))
            #check request successfully
            if (status_code(r)==429){
                head_response=headers(r)
                sleep_time=head_response$`retry-after`
                cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+20,tmcn::toUTF8('\u79D2'),'\n')
                a = Sys.time()
                cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
                cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                    as.character(a+as.numeric(sleep_time)+20),'\n')
                Sys.sleep(as.numeric(sleep_time)+20)
                if (missing(header)) r <- GET(url[i])
                if (!missing(header)) r <- GET(url[i],add_headers(.headers =header))
                if (status_code(r)==429){
                    head_response=headers(r)
                    sleep_time=head_response$`retry-after`
                    cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+40,tmcn::toUTF8('\u79D2'),'\n')
                    a = Sys.time()
                    cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
                    cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                        as.character(a+as.numeric(sleep_time)+20),'\n')
                    Sys.sleep(as.numeric(sleep_time)+40)
                    if (missing(header)) r <- GET(url[i])
                    if (!missing(header)) r <- GET(url[i],add_headers(.headers =header))
                }
            }else if (status_code(r) != 200) {
                return(df)
                stop(status_code(r),
                     tmcn::toUTF8(':\u83B7\u53D6\u7F51\u9875\u51FA\u9519,\u8BF7\u7A0D\u540E\u518D\u5C1D\u8BD5'))
            }
            r_content=content(r)
            # theme page url
            if (grepl('search',url[i])){
                all_items = r_content %>%
                    html_nodes(xpath = '//*[@id="l"]/b[1]') %>%
                    html_text(trim = TRUE) %>%
                    as.numeric()
                if (all_items==0) next(i) # no items in this page
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
                #9. Key, but will not included in df, just to used to judge has abstract or not
                Key=r_content %>%
                    html_nodes(xpath = '//*[@id="resultLst"]//div/p[2]/span[2]/i') %>%
                    html_text(trim = TRUE) %>%
                    gsub(pattern = " ",replacement = "")
                ###data.frame.i
                # df_theme
                if (!missing(subject)) {
                    df_theme = data.frame(subject,study_type,item,person,department,
                                          id,year,fund,abstract_url)
                    colnames(df_theme)=c(tmcn::toUTF8('\u5B66\u79D1\u5206\u7C7B'),
                                         tmcn::toUTF8('\u7814\u7A76\u7C7B\u578B'),
                                         tmcn::toUTF8('\u9879\u76EE\u540D\u79F0'),
                                         tmcn::toUTF8('\u8D1F\u8D23\u4EBA'),
                                         tmcn::toUTF8('\u5355\u4F4D'),
                                         tmcn::toUTF8('\u9879\u76EE\u7F16\u53F7'),
                                         tmcn::toUTF8('\u6279\u51C6\u5E74\u5EA6'),
                                         tmcn::toUTF8('\u91D1\u989D'),
                                         tmcn::toUTF8('\u6458\u8981\u94FE\u63A5'))
                }else{
                    df_theme = data.frame(study_type,item,person,department,
                                          id,year,fund,abstract_url)
                    colnames(df_theme)=c(tmcn::toUTF8('\u7814\u7A76\u7C7B\u578B'),
                                         tmcn::toUTF8('\u9879\u76EE\u540D\u79F0'),
                                         tmcn::toUTF8('\u8D1F\u8D23\u4EBA'),
                                         tmcn::toUTF8('\u5355\u4F4D'),
                                         tmcn::toUTF8('\u9879\u76EE\u7F16\u53F7'),
                                         tmcn::toUTF8('\u6279\u51C6\u5E74\u5EA6'),
                                         tmcn::toUTF8('\u91D1\u989D'),
                                         tmcn::toUTF8('\u6458\u8981\u94FE\u63A5'))
                }
                #divide df_them to two parts: has abstract or not
                if (length(abstract_url[nchar(Key)==0])>0){ #no abastrct
                    cat(tmcn::toUTF8('\u4EE5\u4E0B'),length(abstract_url[nchar(Key)==0]),
                        tmcn::toUTF8('\u4E2A\u6458\u8981\u94FE\u63A5\u65E0\u6458\u8981'),'\n')
                    cat(do::inner_Add_Symbol(abstract_url[nchar(Key)==0],'\n'),'\n')
                }
                df_theme_nokey.i=df_theme[nchar(Key)==0,]
                df_theme_nokey=rbind(df_theme_nokey,df_theme_nokey.i)
                df_theme=df_theme[nchar(Key)!=0,]
                abstract_url=abstract_url[nchar(Key)!=0]
            }else if (grepl('project',url[i])) {
                abstract_url=url[i]
            }
            # scrab abstract part by abstract_url
            if (length(abstract_url)>0){
                for (j in 1:length(abstract_url)) {
                    if (j==1) df_abstract = data.frame()
                    abs_url.j=abstract_url[j]
                    if (missing(header)) r <- GET(abs_url.j)
                    if (!missing(header)) r <- GET(abs_url.j,add_headers(.headers =header))
                    #check request successfully
                    if (status_code(r)==429){
                        head_response=headers(r)
                        sleep_time=head_response$`retry-after`
                        cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+20,tmcn::toUTF8('\u79D2'),'\n')
                        a = Sys.time()
                        cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
                        cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                            as.character(a+as.numeric(sleep_time)+20),'\n')
                        Sys.sleep(as.numeric(sleep_time)+20)
                        if (missing(header)) r <- GET(abs_url.j)
                        if (!missing(header)) r <- GET(abs_url.j,add_headers(.headers =header))
                        if (status_code(r)==429){
                            head_response=headers(r)
                            sleep_time=head_response$`retry-after`
                            cat(tmcn::toUTF8('\u8BF7\u7B49\u5F85'),as.numeric(sleep_time)+20,tmcn::toUTF8('\u79D2'),'\n')
                            a = Sys.time()
                            cat(tmcn::toUTF8('\u73B0\u5728\u65F6\u95F4:'),as.character(a),'\n')
                            cat(tmcn::toUTF8('\u518D\u6B21\u5F00\u59CB\u65F6\u95F4:'),
                                as.character(a+as.numeric(sleep_time)+20),'\n')
                            Sys.sleep(as.numeric(sleep_time)+40)
                            if (missing(header)) r <- GET(abs_url.j)
                            if (!missing(header)) r <- GET(abs_url.j,add_headers(.headers =header))
                        }
                    }else if (status_code(r) != 200) {
                        return(df)
                        stop(status_code(r),
                             tmcn::toUTF8(':\u83B7\u53D6\u7F51\u9875\u51FA\u9519,\u8BF7\u7A0D\u540E\u518D\u5C1D\u8BD5'))
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
                    df_abstract.j=data.frame(subject_abs,time_limit,
                                             key_word_cn,key_word_english,
                                             abs_cn,abs_english)
                    df_abstract=rbind(df_abstract,df_abstract.j)
                    if (j == length(abstract_url)){
                        #rename
                        colnames(df_abstract)=c(tmcn::toUTF8('\u5B66\u79D1\u5206\u7C7B_\u6765\u81EA\u6458\u8981'),
                                                tmcn::toUTF8('\u7814\u7A76\u671F\u9650'),
                                                tmcn::toUTF8('\u4E2D\u6587\u4E3B\u9898\u8BCD'),
                                                tmcn::toUTF8('\u82F1\u6587\u4E3B\u9898\u8BCD'),
                                                tmcn::toUTF8('\u4E2D\u6587\u6458\u8981'),
                                                tmcn::toUTF8('\u82F1\u6587\u6458\u8981')
                        )
                    }
                    if (nchar(abs_cn)<10) cat(i,":",(i-1)*10+j,tmcn::toUTF8('\u6CA1\u6709\u6458\u8981'),
                                              abs_url.j,'\n')
                    if (nchar(abs_cn)>=10) cat(i,":",(i-1)*10+j,'\n')
                }
                # cbind 2 parts
                if (nrow(df_theme)==0) df.i = df_abstract
                if (nrow(df_theme)!=0) df.i = cbind(df_theme,df_abstract)
                df = rbind(df,df.i)
            }
        }
        df=plyr::rbind.fill(df,df_theme_nokey)
        return(df)
    }
}

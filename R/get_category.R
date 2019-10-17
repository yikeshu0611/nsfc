get_category<-function(itemCategory){
    if (!missing(itemCategory)){
        if (is.numeric(itemCategory)){
            if (itemCategory==1)  itemCategory='%E9%9D%A2%E4%B8%8A%E9%A1%B9%E7%9B%AE'
            if (itemCategory==2)  itemCategory='%E9%9D%92%E5%B9%B4%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            if (itemCategory==3)  itemCategory='%E5%9C%B0%E5%8C%BA%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            if (itemCategory==4)  itemCategory='%E5%9B%BD%E9%99%85%28%E5%9C%B0%E5%8C%BA%29%E5%90%88%E4%BD%9C%E4%B8%8E%E4%BA%A4%E6%B5%81%E9%A1%B9%E7%9B%AE'
            if (itemCategory==5)  itemCategory='%E4%B8%93%E9%A1%B9%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            if (itemCategory==6)  itemCategory='%E9%87%8D%E7%82%B9%E9%A1%B9%E7%9B%AE'
            if (itemCategory==7)  itemCategory='%E8%81%94%E5%90%88%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            if (itemCategory==8)  itemCategory='%E9%87%8D%E5%A4%A7%E7%A0%94%E7%A9%B6%E8%AE%A1%E5%88%92'
            if (itemCategory==9)  itemCategory='%E5%BA%94%E6%80%A5%E7%AE%A1%E7%90%86%E9%A1%B9%E7%9B%AE'
            if (itemCategory==10) itemCategory='%E5%9B%BD%E5%AE%B6%E6%9D%B0%E5%87%BA%E9%9D%92%E5%B9%B4%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91'
            if (itemCategory==11) itemCategory='%E4%BC%98%E7%A7%80%E9%9D%92%E5%B9%B4%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            if (itemCategory==12) itemCategory='%E9%87%8D%E5%A4%A7%E9%A1%B9%E7%9B%AE'
            if (itemCategory==13) itemCategory='%E6%B5%B7%E5%A4%96%E5%8F%8A%E6%B8%AF%E6%BE%B3%E5%AD%A6%E8%80%85%E5%90%88%E4%BD%9C%E7%A0%94%E7%A9%B6%E5%9F%BA%E9%87%91'
            if (itemCategory==14) itemCategory='%E5%9B%BD%E5%AE%B6%E5%9F%BA%E7%A1%80%E7%A7%91%E5%AD%A6%E4%BA%BA%E6%89%8D%E5%9F%B9%E5%85%BB%E5%9F%BA%E9%87%91'
            if (itemCategory==15) itemCategory='%E5%88%9B%E6%96%B0%E7%A0%94%E7%A9%B6%E7%BE%A4%E4%BD%93%E9%A1%B9%E7%9B%AE'
            if (itemCategory==16) itemCategory='%E6%B5%B7%E5%A4%96%E6%88%96%E6%B8%AF%E3%80%81%E6%BE%B3%E9%9D%92%E5%B9%B4%E5%AD%A6%E8%80%85%E5%90%88%E4%BD%9C%E7%A0%94%E7%A9%B6%E5%9F%BA%E9%87%91'
            if (itemCategory==17) itemCategory='%E5%9B%BD%E5%AE%B6%E9%87%8D%E5%A4%A7%E7%A7%91%E7%A0%94%E4%BB%AA%E5%99%A8%E7%A0%94%E5%88%B6%E9%A1%B9%E7%9B%AE'
            if (itemCategory==18) itemCategory='%E5%9B%BD%E5%AE%B6%E9%87%8D%E5%A4%A7%E7%A7%91%E7%A0%94%E4%BB%AA%E5%99%A8%E8%AE%BE%E5%A4%87%E7%A0%94%E5%88%B6%E4%B8%93%E9%A1%B9'
            if (itemCategory==19) itemCategory='%E5%88%9B%E6%96%B0%E7%A0%94%E7%A9%B6%E7%BE%A4%E4%BD%93%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91'
            if (itemCategory==20) itemCategory='%E7%A7%91%E5%AD%A6%E4%B8%AD%E5%BF%83%E9%A1%B9%E7%9B%AE'
            if (itemCategory==21) itemCategory='%E5%85%B6%E4%BB%96'
        }else{
            if (itemCategory==tmcn::toUTF8('\u9762\u4E0A\u9879\u76EE')){
                itemCategory='%E9%9D%A2%E4%B8%8A%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u9752\u5E74\u79D1\u5B66\u57FA\u91D1\u9879\u76EE')){
                itemCategory='%E9%9D%92%E5%B9%B4%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u5730\u533A\u79D1\u5B66\u57FA\u91D1\u9879\u76EE')){
                itemCategory='%E5%9C%B0%E5%8C%BA%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u56FD\u9645(\u5730\u533A)\u5408\u4F5C\u4E0E\u4EA4\u6D41\u9879\u76EE')){
                itemCategory='%E5%9B%BD%E9%99%85%28%E5%9C%B0%E5%8C%BA%29%E5%90%88%E4%BD%9C%E4%B8%8E%E4%BA%A4%E6%B5%81%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u4E13\u9879\u57FA\u91D1\u9879\u76EE')){
                itemCategory='%E4%B8%93%E9%A1%B9%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u91CD\u70B9\u9879\u76EE')){
                itemCategory='%E9%87%8D%E7%82%B9%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u8054\u5408\u57FA\u91D1\u9879\u76EE')){
                itemCategory='%E8%81%94%E5%90%88%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u91CD\u5927\u7814\u7A76\u8BA1\u5212')){
                itemCategory='%E9%87%8D%E5%A4%A7%E7%A0%94%E7%A9%B6%E8%AE%A1%E5%88%92'
            }else if (itemCategory==tmcn::toUTF8('\u5E94\u6025\u7BA1\u7406\u9879\u76EE')){
                itemCategory='%E5%BA%94%E6%80%A5%E7%AE%A1%E7%90%86%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u56FD\u5BB6\u6770\u51FA\u9752\u5E74\u79D1\u5B66\u57FA\u91D1')){
                itemCategory='%E5%9B%BD%E5%AE%B6%E6%9D%B0%E5%87%BA%E9%9D%92%E5%B9%B4%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91'
            }else if (itemCategory==tmcn::toUTF8('\u4F18\u79C0\u9752\u5E74\u79D1\u5B66\u57FA\u91D1\u9879\u76EE')){
                itemCategory='%E4%BC%98%E7%A7%80%E9%9D%92%E5%B9%B4%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u91CD\u5927\u9879\u76EE')){
                itemCategory='%E9%87%8D%E5%A4%A7%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u6D77\u5916\u53CA\u6E2F\u6FB3\u5B66\u8005\u5408\u4F5C\u7814\u7A76\u57FA\u91D1')){
                itemCategory='%E6%B5%B7%E5%A4%96%E5%8F%8A%E6%B8%AF%E6%BE%B3%E5%AD%A6%E8%80%85%E5%90%88%E4%BD%9C%E7%A0%94%E7%A9%B6%E5%9F%BA%E9%87%91'
            }else if (itemCategory==tmcn::toUTF8('\u56FD\u5BB6\u57FA\u7840\u79D1\u5B66\u4EBA\u624D\u57F9\u517B\u57FA\u91D1')){
                itemCategory='%E5%9B%BD%E5%AE%B6%E5%9F%BA%E7%A1%80%E7%A7%91%E5%AD%A6%E4%BA%BA%E6%89%8D%E5%9F%B9%E5%85%BB%E5%9F%BA%E9%87%91'
            }else if (itemCategory==tmcn::toUTF8('\u521B\u65B0\u7814\u7A76\u7FA4\u4F53\u9879\u76EE')){
                itemCategory='%E5%88%9B%E6%96%B0%E7%A0%94%E7%A9%B6%E7%BE%A4%E4%BD%93%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u6D77\u5916\u6216\u6E2F\u3001\u6FB3\u9752\u5E74\u5B66\u8005\u5408\u4F5C\u7814\u7A76\u57FA\u91D1')){itemCategory='%E6%B5%B7%E5%A4%96%E6%88%96%E6%B8%AF%E3%80%81%E6%BE%B3%E9%9D%92%E5%B9%B4%E5%AD%A6%E8%80%85%E5%90%88%E4%BD%9C%E7%A0%94%E7%A9%B6%E5%9F%BA%E9%87%91'
            }else if (itemCategory==tmcn::toUTF8('\u56FD\u5BB6\u91CD\u5927\u79D1\u7814\u4EEA\u5668\u7814\u5236\u9879\u76EE')){
                itemCategory='%E5%9B%BD%E5%AE%B6%E9%87%8D%E5%A4%A7%E7%A7%91%E7%A0%94%E4%BB%AA%E5%99%A8%E7%A0%94%E5%88%B6%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u56FD\u5BB6\u91CD\u5927\u79D1\u7814\u4EEA\u5668\u8BBE\u5907\u7814\u5236\u4E13\u9879')){
                itemCategory='%E5%9B%BD%E5%AE%B6%E9%87%8D%E5%A4%A7%E7%A7%91%E7%A0%94%E4%BB%AA%E5%99%A8%E8%AE%BE%E5%A4%87%E7%A0%94%E5%88%B6%E4%B8%93%E9%A1%B9'
            }else if (itemCategory==tmcn::toUTF8('\u521B\u65B0\u7814\u7A76\u7FA4\u4F53\u79D1\u5B66\u57FA\u91D1')){
                itemCategory='%E5%88%9B%E6%96%B0%E7%A0%94%E7%A9%B6%E7%BE%A4%E4%BD%93%E7%A7%91%E5%AD%A6%E5%9F%BA%E9%87%91'
            }else if (itemCategory==tmcn::toUTF8('\u79D1\u5B66\u4E2D\u5FC3\u9879\u76EE')){
                itemCategory='%E7%A7%91%E5%AD%A6%E4%B8%AD%E5%BF%83%E9%A1%B9%E7%9B%AE'
            }else if (itemCategory==tmcn::toUTF8('\u5176\u4ED6')){
                itemCategory='%E5%85%B6%E4%BB%96'
            }else{
                message(tmcn::toUTF8('\u5B81\u7684\u8F93\u5165\u4E0D\u6B63\u786E\u5455!\u8BF7\u4ECE\u4E0B\u9762\u9009\u62E91\u4E2A\u5427!'))
                message(tmcn::toUTF8('\u6CE8\u610F:\u662F\u8F93\u51651\u4E2A\u6570\u5B57\u5455!'))

                    cat(' 1',':',tmcn::toUTF8('\u9762\u4E0A\u9879\u76EE'),'\n')
                    cat(' 2',':',tmcn::toUTF8('\u9752\u5E74\u79D1\u5B66\u57FA\u91D1\u9879\u76EE'),'\n')
                    cat(' 3',':',tmcn::toUTF8('\u5730\u533A\u79D1\u5B66\u57FA\u91D1\u9879\u76EE'),'\n')
                    cat(' 4',':',tmcn::toUTF8('\u56FD\u9645(\u5730\u533A)\u5408\u4F5C\u4E0E\u4EA4\u6D41\u9879\u76EE'),'\n')
                    cat(' 5',':',tmcn::toUTF8('\u4E13\u9879\u57FA\u91D1\u9879\u76EE'),'\n')
                    cat(' 6',':',tmcn::toUTF8('\u91CD\u70B9\u9879\u76EE'),'\n')
                    cat(' 7',':',tmcn::toUTF8('\u8054\u5408\u57FA\u91D1\u9879\u76EE'),'\n')
                    cat(' 8',':',tmcn::toUTF8('\u91CD\u5927\u7814\u7A76\u8BA1\u5212'),'\n')
                    cat(' 9',':',tmcn::toUTF8('\u5E94\u6025\u7BA1\u7406\u9879\u76EE'),'\n')
                    cat(' 10',':',tmcn::toUTF8('\u56FD\u5BB6\u6770\u51FA\u9752\u5E74\u79D1\u5B66\u57FA\u91D1'),'\n')
                    cat(' 11',':',tmcn::toUTF8('\u4F18\u79C0\u9752\u5E74\u79D1\u5B66\u57FA\u91D1\u9879\u76EE'),'\n')
                    cat(' 12',':',tmcn::toUTF8('\u91CD\u5927\u9879\u76EE'),'\n')
                    cat(' 13',':',tmcn::toUTF8('\u6D77\u5916\u53CA\u6E2F\u6FB3\u5B66\u8005\u5408\u4F5C\u7814\u7A76\u57FA\u91D1'),'\n')
                    cat(' 14',':',tmcn::toUTF8('\u56FD\u5BB6\u57FA\u7840\u79D1\u5B66\u4EBA\u624D\u57F9\u517B\u57FA\u91D1'),'\n')
                    cat(' 15',':',tmcn::toUTF8('\u521B\u65B0\u7814\u7A76\u7FA4\u4F53\u9879\u76EE'),'\n')
                    cat(' 16',':',tmcn::toUTF8('\u6D77\u5916\u6216\u6E2F\u3001\u6FB3\u9752\u5E74\u5B66\u8005\u5408\u4F5C\u7814\u7A76\u57FA\u91D1'),'\n')
                    cat(' 17',':',tmcn::toUTF8('\u56FD\u5BB6\u91CD\u5927\u79D1\u7814\u4EEA\u5668\u7814\u5236\u9879\u76EE'),'\n')
                    cat(' 18',':',tmcn::toUTF8('\u56FD\u5BB6\u91CD\u5927\u79D1\u7814\u4EEA\u5668\u8BBE\u5907\u7814\u5236\u4E13\u9879'),'\n')
                    cat(' 19',':',tmcn::toUTF8('\u521B\u65B0\u7814\u7A76\u7FA4\u4F53\u79D1\u5B66\u57FA\u91D1'),'\n')
                    cat(' 20',':',tmcn::toUTF8('\u79D1\u5B66\u4E2D\u5FC3\u9879\u76EE'),'\n')
                    cat(' 21',':',tmcn::toUTF8('\u5176\u4ED6'),'\n')


                form <- list(
                    ":NUM"=tmcn::toUTF8('\u8BF7\u9009\u62E91\u4E2A\u6570\u5B57')
                )

                number_form=svDialogs::dlg_form(form)$res
                itemCategory<-as.numeric(as.character(unlist(number_form)))
            }
        }
        return(itemCategory)
    }
}

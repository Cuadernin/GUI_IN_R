require(tcltk)
require(tcltk2)
require(glue)
require(fOptions)
require(float)

binomial_mod=function(){
  S=tclVar("51")   # define values to show 
  K=tclVar("50")
  T=tclVar("3/12")
  r=tclVar("0.10")
  sigma=tclVar("0.30")
  n=tclVar("3")
  tt=tktoplevel()  
  tkwm.title(tt,"CALL-PUT") # window title
  tkwm.resizable(tt,FALSE,FALSE)  # no resizable
  
  am=tkradiobutton(tt) # create two radiobuttons
  eu=tkradiobutton(tt)
  option=tclVar("eu")     # default option
  tkconfigure(am,variable=option,value="am")
  tkconfigure(eu,variable=option,value="eu")  
  S.entry=tkentry(tt,textvariable=S,bg='white',fg='red',justify='center')
  K.entry=tkentry(tt,textvariable=K,bg='white',fg='red',justify='center') # define entrys
  T.entry=tkentry(tt,textvariable=T,bg='white',fg='red',justify='center')
  r.entry=tkentry(tt,textvariable=r,bg='white',fg='blue',justify='center')
  sigma.entry=tkentry(tt,textvariable=sigma,bg='white',fg='blue',justify='center')
  n.entry=tkentry(tt,textvariable=n,bg='white',fg='blue',justify='center')
  tk2tip(S.entry,"Stock price. It must be positive.")
  tk2tip(K.entry,"Strike price. It must be positive.")  # create tooltips for entrys
  tk2tip(T.entry,"Life of option. It must be positive.")
  tk2tip(r.entry,"Risk free. It must be between 0 and 1.")
  tk2tip(sigma.entry,"Volatility of stock price. It must be between 0 and 1.")
  tk2tip(n.entry,"Number of periods. It must be integer and positive.")
  
  reset=function() {
    tclvalue(S)=""
    tclvalue(K)=""   # define a function for reset values
    tclvalue(T)=""
    tclvalue(r)=""
    tclvalue(sigma)=""
    tclvalue(n)=""}
  
  reset.but=tkbutton(tt,text="Reset",command=reset,cursor="hand1",width=5) # reset button
  calculate=function(){
    S=as.numeric(tclvalue(S))   # read values
    K=as.numeric(tclvalue(K)) 
    T=tclvalue(T)               # read values 
    T=sapply(T,function(x)eval(parse(text=x)))
    r=as.numeric(tclvalue(r))
    sigma=as.numeric(tclvalue(sigma))
    n=as.integer(tclvalue(n))
    opt=tclvalue(option)
    
    if(S>0 & K>0 & T>0 & r>0 & n>0){ 
      tt2=tktoplevel()   # create new window
      tkwm.title(tt2,"Results") 
      tkwm.resizable(tt2,FALSE,FALSE)   # no resizable
      quit2.but=tkbutton(tt2,text="Quit",command=function(){tkdestroy(tt2)},cursor="hand2",width=7,fg="white")  # quit button (new window)
      
      title2=tkfont.create(size=14,weight="bold",family="Tahoma")     # create new fonts
      txt2=tkfont.create(size=12,family="Times New Roman",weight="bold")  
      label2=tklabel(tt2,text="Results")    # create label
      txt_results=tkfont.create(size=10,family="Cambria")    # create new fonts
      button_txt2=tkfont.create(size=10,family="Helvetica",weight="bold")
      
      tkconfigure(label2,font=title2)    #change label2 font
      tkconfigure(quit2.but,bg="#FE2E2E",font=button_txt2) #config quit button

      am_eu=ifelse(opt=="am",TRUE,FALSE) 
      b=bino2(S,K,T,r,n,sigma,TRUE,am_eu) # binomial model
          if(am_eu){ # if american
            put=b
            call=bino2(S,K,T,r,n,sigma,FALSE,am_eu)
            tkgrid(label1,columnspan=3,pady=9)
            tkgrid(tklabel(tt2,text="American Put",font=txt2),tklabel(tt2,text=put,font=txt_results),pady=3,padx=1)  # showing results
            tkgrid(tklabel(tt2,text="American Call",font=txt2),tklabel(tt2,text=call,font=txt_results),pady=3,padx=1)
            tkgrid(quit2.but,columnspan=3,pady=9)}
          else{
            put=b
            call=put-K*exp(-r*T)+S
            tkgrid(label1,columnspan=3,pady=9)
            tkgrid(tklabel(tt2,text="Put",font=txt2),tklabel(tt2,text=put,font=txt_results),pady=3,padx=1)  # showing results
            tkgrid(tklabel(tt2,text="Call",font=txt2),tklabel(tt2,text=call,font=txt_results),pady=3,padx=1)
            tkgrid(tklabel(tt2,text="Equality Put - Call",font=txt2),tklabel(tt2,text=glue("{put+S} = {call+K*exp(-r*T)}"), 
            font=txt_results),pady=9,padx=8)  # like f strings 
            tkgrid(quit2.but,columnspan=3,pady=9)
          }
        }
    else{
      tk_messageBox(type="ok",message="Values must be positive")
    }
  }
  calculate.but=tkbutton(tt,text="Calculate",command=calculate,cursor="hand2",width=9) # calculate button
  quit.but=tkbutton(tt,text="Quit", command=function(){tkdestroy(tt)},cursor="hand2",fg="white",width=5) # quit button
  
  txt=tkfont.create(size=11,family="Times New Roman")
  title=tkfont.create(size=14,weight="bold",family="Tahoma")  # create new fonts
  button_txt=tkfont.create(size=11,weight="bold",family="Helvetica")
  sbtitle=tkfont.create(size=10,weight="bold",family="Georgia")
  entry_txt=tkfont.create(size=10,family="Cambria")
  
  label1=tklabel(tt,text="Write your variables")    # title

  tkconfigure(calculate.but,bg="#2EFE2E",font=button_txt) 
  tkconfigure(quit.but,bg="#FE2E2E",font=button_txt)
  tkconfigure(reset.but,bg="#00FFFF",font=button_txt)
  tkconfigure(label1,font=title)
  
  tkconfigure(S.entry,font=entry_txt)   # change entrys fonts
  tkconfigure(K.entry,font=entry_txt)
  tkconfigure(T.entry,font=entry_txt)
  tkconfigure(r.entry,font=entry_txt) 
  tkconfigure(sigma.entry,font=entry_txt)
  tkconfigure(n.entry,font=entry_txt)
  
  tkgrid(label1,columnspan=3,pady=9)   # define element ubication
  tkgrid(tklabel(tt,text="S (S_0)",font=txt), S.entry,pady=9,padx=8)
  tkgrid(tklabel(tt,text="Strike Price (K)",font=txt),K.entry,pady=9,padx=8)
  tkgrid(tklabel(tt,text="Time (T)",font=txt),T.entry,pady=9,padx=8)
  tkgrid(tklabel(tt,text="r",font=txt), r.entry,pady=9,padx=8)
  tkgrid(tklabel(tt,text="Volatility (Sigma)",font=txt),sigma.entry,pady=9,padx=8)
  tkgrid(tklabel(tt,text="Periods (n)",font=txt),n.entry,pady=9,padx=8)
  
  label3=tklabel(tt,text="American or European?")
  tkconfigure(label3,font=sbtitle)
  
  tkgrid(label3,column=1)
  tkgrid(tklabel(tt,text="Am",font=txt),am)
  tkgrid(tklabel(tt,text="Eu",font=txt),eu)
  tkgrid(calculate.but,reset.but,quit.but,pady=8,padx=8)
}
binomial_mod()

FUNCTION read_table, filename, delimiter, count = count, field_name = field_name, field_type = field_type
  head='' &  type=''
  OPENR, lun, filename, /get_lun
  READF, lun, head &  READF, lun, type
  FREE_LUN, lun
  PRINT,head
  IF ~KEYWORD_SET(delimiter) THEN delimiter = ','
  field_name=strsplit(head,delimiter,/extract, count=fieldcount)
  ;field_type=fix(strsplit(type,delimiter,/extract))
  field_type = INTARR(N_ELEMENTS(field_name))& field_type[*]=7

  tmpl  =  {$
    VERSION:1.  ,$
    DATASTART:2,$
    DELIMITER:delimiter,$
    MISSINGVALUE:!VALUES.F_NAN ,$
    COMMENTSYMBOL:'',$
    FIELDCOUNT:FIELDCOUNT,$
    FIELDTYPES:field_type,$
    FIELDNAMES:field_name,$
    FIELDLOCATIONS:LONARR(FIELDCOUNT),$
    FIELDGROUPS:LINDGEN(FIELDCOUNT)$
  }
  temp = read_ascii(Filename,count = count,template = tmpl)
  RETURN, temp

END

;统计每个月的累计降雨量Rain是多少，以及每个月有效值的比率（-6999是无效值，不参与统计）
PRO rain,data
  x=INDGEN(12)
  y1=fltarr(12)
  y2=fltarr(12)
tnums=data.DATE.LENGTH
month=1
monthlyrain=0
monthlyinvalid=0
times=0
FOR i=0,tnums-1 DO BEGIN
  times++
  cm=FIX(STRMID(data.DATE[i],4,2))
  IF cm EQ month THEN BEGIN
    IF data.MS_2M[i] EQ -6999 THEN BEGIN
      monthlyinvalid++
    ENDIF ELSE BEGIN
      monthlyrain+=float(data.RAIN[i])
      
    ENDELSE
    IF i EQ tnums-1 THEN BEGIN
      ratio=1-float(monthlyinvalid)/times
      ;PRINT,month,'月的累计降水量为:',monthlyrain,'    有效值比率为:',ratio
      y1[month-1]=monthlyrain
      y2[month-1]=ratio
    ENDIF
  ENDIF ELSE BEGIN
    ratio=1-float(monthlyinvalid)/times
    ;PRINT,month,'月的累计降水量为:',monthlyrain,'    有效值比率为:',ratio
    y1[month-1]=monthlyrain
    y2[month-1]=ratio
    month++
    monthlyrain=0
    monthlyinvalid=0
    times=0
  ENDELSE
ENDFOR

for i=1,month do begin
  PRINT,x[i-1]+1,'月的累计降水量为:',y1[i-1],'    有效值比率为:',y2[i-1]
endfor
;plot,[1:12],y1,xtitle='Month',ytitle='Rain'
;oplot,[1:12],y2
END

;统计2cm-160cm各层深度的土壤水分（Ms_2m至Ms_160m）的逐旬（每月1-10号为上旬，11-20是中旬，21-月末最后一天是下旬）平均值，
;以及每旬的有效值的比率。
PRO soilmois,data,soilmoisture,feature
  m=0
  n=0
  
  y1=FLTARR(36)
  y2=FLTARR(36)
  tnums=data.DATE.LENGTH
  month=1
  tenday=1
  tendaysoilmoisture=0
  tendayinvalid=0
  times=0
  print,'-----------------------------------------------------------------------------------------------------'
  print,'  传感器为',feature,'深度时:'
  FOR i=0,tnums-1 DO BEGIN
    times++
    cm=FIX(STRMID(data.DATE[i],4,2))
    cd=FIX(STRMID(data.DATE[i],6,2))
  
  ;cd控制上中下旬  
    if cd le 10 then begin 
      ftenday=1
    endif  else if cd le 20 then begin 
      ftenday=2
    endif  else begin 
      ftenday=3 
    endelse
     
    IF cm EQ month THEN BEGIN
      IF ftenday EQ tenday THEN BEGIN
        IF data.MS_2M[i] EQ -6999 THEN BEGIN
          tendayinvalid++
        ENDIF ELSE BEGIN
          tendaysoilmoisture+=float(soilmoisture[i])
        ENDELSE
          IF i EQ tnums-1 THEN BEGIN
            ratio=1-float(tendayinvalid)/times
            average=float(tendaysoilmoisture)/times
            PRINT,month,'月下旬土壤平均水分为:',average,'    有效值比率为:',ratio
            y1[m]=average
            y2[n]=ratio
          ENDIF
      ENDIF ELSE BEGIN
        ratio=1-float(tendayinvalid)/times
        average=float(tendaysoilmoisture)/times
          if tenday eq 1 then begin
          PRINT,month,'月上旬土壤平均水分为:',average,'    有效值比率为:',ratio
          y1[m++]=average
          y2[n++]=ratio
          endif  else if tenday eq 2 then begin
          PRINT,month,'月中旬土壤平均水分为:',average,'    有效值比率为:',ratio
          y1[m++]=average
          y2[n++]=ratio
          endif  
        tenday++
        tendaysoilmoisture=0
        tendayinvalid=0
        times=0
          
      ENDELSE
    endif ELSE BEGIN
      ratio=1-float(tendayinvalid)/times
      average=float(tendaysoilmoisture)/times
      PRINT,month,'月下旬土壤平均水分为:',average,'    有效值比率为:',ratio
      y1[m++]=average
      y2[n++]=ratio
      tenday=1
      tendaysoilmoisture=0
      tendayinvalid=0
      times=0
      month++
      i--
    ENDELSE
  ENDFOR
  ;plot,[1:36],y1,xtitle='tendays',ytitle='soilmoisture'
  ;OPLOT,[1:36],y2
END

PRO homework01
  csv = FILE_DIRNAME(ROUTINE_FILEPATH('homework01'))+'/2013.csv'
  data = read_table(csv)

 rain,data
 soilmois,data,data.Ms_2m,'2cm'
 soilmois,data,data.Ms_4m,'4cm'
 soilmois,data,data.Ms_10m,'10cm'
 soilmois,data,data.Ms_20m,'20cm'
 soilmois,data,data.Ms_40m,'40cm'
 soilmois,data,data.Ms_80m,'80cm'
 soilmois,data,data.Ms_120m,'120cm'
 soilmois,data,data.Ms_160m,'160cm'
   

END
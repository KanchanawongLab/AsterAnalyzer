pro initializeNODE, wwidget
common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array
common node_mask, ImageROI_ptrarr_ptrarr
common display, wxszx, wyszx, mainwindow, zoomcoord, autoscale, screenmode
  
  wxsz = 1024. & wysz = 1024.
  xydsz = [1024,1024]
  def_w = !D.WINDOW
  frontendwindow= !D.WINDOW
  mousedown = 0
  mainwindow = !D.WINDOW

  frontendzoom = [0,0,1023,1023]
  boxcolor = !D.N_colors-10
  info = {image: dblarr(2,2), $
    wid:frontendwindow, $
    drawID: widget_info(wWidget,find_by_uname='WID_DRAW_MAIN'), $
    pixID:-1, $
    xsize:wxsz, $
    ysize:wysz, $
    sx:-1,$
    sy:-1,$
    boxColor: boxColor}
  infoinitial = info
  widget_control, wWidget, set_uvalue =info, /no_copy
  
  print, 'initialized..'
  device,decompose=0
  cgloadct, 0, /reverse
  
  node_table = dblarr(19,10)
  properties = {datafile: '',timestamp: 0.,xpixels:512.,ypixels:512.,frames:1.}
  widget_control,widget_info(wwidget,find_by_uname='WID_DROPLIST_COLOR'),set_droplist_select=3
end

pro WID_BASE_node_event, Event
common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array

  wTarget = (widget_info(Event.id,/NAME) eq 'TREE' ?  $
    widget_info(Event.id, /tree_root) : event.id)
  wWidget =  Event.top
  
  wxsz = 1024
  wysz = 1024
  
 ; colordroplist=['Red Temperature # 3','Grayscale # 0','Rainbow # 13','Inverse Grayscale','Rainbow #39','Brewer Red-Blue 22']
  colordroplist=[3,0,13,0,39,22]
  displaylist = ['Raw','Mask','Overlay']
  ;edgelist = ['Sobel','Roberts','Prewitt','Emboss','Edge_DOG','Laplacian','Unsharpmask']
  widget_control, Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_FRAME'),get_value=frame
  colorchoice = widget_info(Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_COLOR'),/droplist_select)
  mousemode = widget_info(Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_MOUSE'),/droplist_select)
 displaymode = widget_info(Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_DISPLAY'),/droplist_select)
 maskmode = widget_info(Widget_Info(wWidget, FIND_BY_UNAME='WID_DROPLIST_DETECTION'),/droplist_select)+1
 
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_GAUSSIANSIGMA'),get_value=gaussian_sigma
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_WINDOWRADIUS'),get_value=window_radius
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_CAMERAPIXELMICRON'),get_value=camera_pixel_micron
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_MAXPEAKS'),get_value=max_peaks
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_WAVELENGTH'),get_value=wavelength
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_MAGNIFICATION'),get_value=  magnification
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_NA'),get_value=na
  
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_ASTERRADIUS'),get_value=asterradius
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_DARKCOUNT'),get_value=darkcount
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_TEXT_ASTERID'),get_value=asterid
  widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_THRESHOLD'),get_value=threshold

  case wTarget of
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXIT'):  begin
         widget_control, event.top,/destroy
        ; wset, def_w
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_DRAW_MAIN'): drawevents,event
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_CLEARSCREEN'):tvscl,bytarr(1024,1024) 
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_COPYSCREEN'):begin
      presentimage=tvrd(true=1)
      cgwindow, wxsize = 1024., wysize = 1024.
      cgimage, presentimage,position=[0.,0.,1.,1.],/addcmd, /keep_aspect_ratio,/interpolate
    end
    widget_info(wWidget, FIND_BY_UNAME='WID_DROPLIST_COLOR'):      case colorchoice of
        3: cgloadct, 0,/reverse
        5: cgloadct, 1,/reverse
        else: cgloadct, colordroplist[colorchoice]
      endcase  
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_LOADTIFF'):begin 
      node_loadtiff, event
      node_display,event,frame=frame
    end 
    Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_FRAME'): begin
      case displaymode of 
        0: node_display, event, frame=frame
        1: node_display, event, frame=frame
        2: node_display, event, frame=frame,/maskoverlay
      endcase      
      if n_elements(node_table_array) lt 1 then return
      if ptr_valid(node_table_array[frame-1]) then begin
        node_table = *(node_table_array[frame-1])
        widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_TABLE_NODES'),set_value=node_table
      endif
      
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SAVESCREEN'):   savescreentiff, event
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SAVESAV'):node_io,event,/savesav
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_LOADSAV'):begin
      node_io,event,/loadsav
     if n_elements(node_table_array) lt 1 then return
      if ptr_valid(node_table_array[frame-1]) then begin
      node_table = *(node_table_array[frame-1])
      widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_TABLE_NODES'),set_value=node_table
     endif
   end   
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_RESETZOOM'):begin
      node_display,event,/reset
      node_display,event,frame=frame
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_REFRESH'):begin
    case displaymode of 
        0: node_display, event, frame=frame
        1: node_display, event, frame=frame
        2: node_display, event, frame=frame,/maskoverlay
    endcase  
  end
  Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_REFRESH2'):begin
  case displaymode of
    0: node_display, event, frame=frame
    1: node_display, event, frame=frame
    2: node_display, event, frame=frame,/maskoverlay
  endcase
end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_ZOOMOUT2X'):begin
      node_display,event,/out2x
      node_display,event,frame=frame
    end
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_DETECT'):begin
      print,'Detect Cortical asters:'
      thisframeindex = (frame-1>0)<(properties.frames-1)
      z=node_getzoomfactor()
      node_mask, event,get_mask=frame, mask=mask
      node_findPeaks,image=reform(image_stack[z.xmin:z.xmax,z.ymin:z.ymax,thisframeindex]),/verbose, Gauss_sigma=gaussian_sigma,maxcount=max_peaks,results=node_searchbuffer, $
        zoomfactor=z, usemask = (maskmode eq 1), mask=mask, frame=frame
      ;help, node_searchbuffer      
      node_table,event,/update
      node_table,event,minpeakgray=minpeakgray
      node_table, event,/refresh
    end 
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_DETECTALL'):begin
      z=node_getzoomfactor()
      numframes = n_elements(node_table_array)
      if numframes lt 1 then return
      print,'Detect Cortical asters in all frames:'
      for i = 1, numframes do begin
        widget_control, Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_FRAME'),set_value=(1)
        node_display, event, frame=i
        node_mask, event,get_mask=i, mask=mask
        node_findPeaks,image=reform(image_stack[z.xmin:z.xmax,z.ymin:z.ymax,i-1]),/verbose, Gauss_sigma=gaussian_sigma,maxcount=max_peaks,results=node_searchbuffer, $
          zoomfactor=z, usemask = (maskmode eq 1), mask=mask, frame=i
        node_table,event,/update        
        node_table,event,minpeakgray=minpeakgray
        node_table, event,/refresh
        node_table_array[i-1] = ptr_new(node_table)  
      endfor
      print,'Finish..'
      msg=dialog_message('Finish analysis all frames')
      
    end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_AUTOPARAM'):begin
      gaussian_sigma = (0.21*wavelength/na)/(camera_pixel_micron*1000./magnification)
      print, 'PSF sigma is:',gaussian_sigma
      widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_GAUSSIANSIGMA'),set_value =  gaussian_sigma
      window_radius = ceil(gaussian_sigma)*4
      widget_control,Widget_Info(wWidget, FIND_BY_UNAME='WID_SLIDER_WINDOWRADIUS'),set_value =window_radius
  
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_PINPOINT'):begin
      selectedrow = (widget_info(widget_info(event.top,find_by_uname='WID_TABLE_NODES'),/table_select))[1,*]
      sel = selectedrow(uniq(selectedrow))
      ;help, sel
      node_table, event, /pinpoint, selection=sel
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_ZOOMNODE'):
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_REFRESHTABLE'):node_table, event,/refresh
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_REMOVENODE'):begin
        selectedrow = (widget_info(widget_info(event.top,find_by_uname='WID_TABLE_NODES'),/table_select))[1,*]
        sel = selectedrow(uniq(selectedrow))
     ;help, sel
     node_table, event, /delete, selection=sel
     widget_control,widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_table_select=[-1,-1]
    end 
    Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_REMOVENODEID'):begin     
      removeid = node_stringtokenizer(asterid)
      node_table, event, /delete, id=removeid
      print,'Remove Aster:', removeid
    end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXPORTTABLE'):
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SORTTABLE'):begin
      selectedcol = (widget_info(widget_info(event.top,find_by_uname='WID_TABLE_NODES'),/table_select))[*,1]
      sel = (selectedcol(uniq(selectedrow)))[0]
      if sel lt 0 then return
      if n_elements(node_table) eq 0 then return
      coltosort = reform(node_table[sel,*])
      node_table=node_table[*,reverse(sort(coltosort))]
      widget_control,widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
      node_table_array[frame-1]=ptr_new(node_table)
     end     
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_HISTOGRAMCOLUMN'):begin
      if n_elements(node_table) eq 0 then return
      ntsize = size(node_table,/dimensions)
      numasters = ntsize[1]
      selectedcol = (widget_info(widget_info(event.top,find_by_uname='WID_TABLE_NODES'),/table_select))[*,1]
      sel = (selectedcol(uniq(selectedrow)))[0]
      if sel lt 0 then return
      cgimage, bytarr(100,100),position = [0.0,0.0,0.45,0.25],/noerase
      if n_elements(sel) eq 1 then begin
        coltohisto = reform(node_table[sel,*])
        if n_elements(coltohisto) lt 50 then nbins = 10 else nbins = 25        
        cghistoplot,coltohisto,/noerase,nbins=nbins,axiscolorname='red',/nan,/fillpolygon, position=[0.05,0.05,0.45,0.25]      
        endif
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_FULLSTAT1'):begin
      if n_elements(node_table) eq 0 then return
       cgplot, node_table[6,*],node_table[3,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='meanPeak',ytitle='maxPeak',position=[0.075,0.05,0.315,0.32]
       peakr = correlate(node_table[6,*],node_table[3,*])      
       cgtext, 0.1,0.34,'R (Pearson) : '+string(peakr,format='(F10.4)'),color='red',/normal 
       cgplot, node_table[4,*],node_table[7,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='meanBkgnd',ytitle='BkgndSigma',position=[0.395,0.05,0.635,0.32]
       backr = correlate(node_table[4,*],node_table[7,*])
       cgtext, 0.4,0.34,'R (Pearson) : '+string(backr,format='(F10.4)'),color='red',/normal 
       cgplot, node_table[5,*],node_table[9,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='meanP/B',ytitle='maxP/B',position=[0.72,0.05,0.995,0.32]
       maxr = correlate(node_table[5,*],node_table[9,*])
       cgtext, 0.75,0.34,'R (Pearson) : '+string(maxr,format='(F10.4)'),color='red',/normal 
       
       if n_elements(coltohisto) lt 50 then nbins = 10 else nbins = 25
       cghistoplot,node_table[3,*],/noerase,nbins=nbins,axiscolorname='red',/nan, position=[0.075,0.45,0.315,0.75],xtitle='maxPeak',/fillpolygon
       cghistoplot,node_table[4,*],/noerase,nbins=nbins,axiscolorname='red',/nan,position=[0.395,0.45,0.635,0.75],xtitle='meanBkgnd',/fillpolygon
       cghistoplot,node_table[9,*],/noerase,nbins=nbins,axiscolorname='red',/nan, position=[0.72,0.45,0.995,0.75],xtitle='maxP/B',/fillpolygon
       cgtext, 0.1,0.95,'Rep. MaxPeak: '+string(mean(/nan,node_table[3,*] ),format='(F10.4)'),color='red',/normal 
       cgtext, 0.4,0.95,'Rep. Bkgnd: '+string(mean(/nan,node_table[4,*] ),format='(F10.4)'),color='red',/normal 
       cgtext, 0.75,0.95,'Rep. mxP/B: '+string(mean(/nan,node_table[9,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.1,0.92,'Max. MaxPeak: '+string(max(/nan,node_table[3,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.4,0.92,'Max. Bkgnd: '+string(max(/nan,node_table[4,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.75,0.92,'Max. mxP/B: '+string(max(/nan,node_table[9,*] ),format='(F10.4)'),color='red',/normal 
       cgtext, 0.1,0.89,'Min MaxPeak: '+string(min(/nan,node_table[3,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.4,0.89,'Min Bkgnd: '+string(min(/nan,node_table[4,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.75,0.89,'Min mxP/B): '+string(min(/nan,node_table[9,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.1,0.86,'Std MaxPeak: '+string(stddev(/nan,node_table[3,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.4,0.86,'Std Bkgnd: '+string(stddev(/nan,node_table[4,*] ),format='(F10.4)'),color='red',/normal
       cgtext, 0.75,0.86,'Std mxP/B: '+string(stddev(/nan,node_table[9,*] ),format='(F10.4)'),color='red',/normal
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_FULLSTAT2'):begin
        node_quantify_spatial, node_table=node_table, properties=properties,/verbose
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_FULLSTAT3'):begin
        if n_elements(node_table) eq 0 then return
        cgplot, node_table[9,*],node_table[3,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='maxP/B',ytitle='maxPeak',position=[0.09,0.05,0.315,0.32]
        maxpbmaxpeakr = correlate(node_table[9,*],node_table[3,*])
        cgtext, 0.1,0.34,'R (Pearson) : '+string(maxpbmaxpeakr,format='(F10.4)'),color='red',/normal 
        cgplot, node_table[9,*],node_table[4,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='maxP/B',ytitle='meanBkGnd',position=[0.395,0.05,0.635,0.32]
        maxpbckgnd = correlate(node_table[9,*],node_table[4,*])
        cgtext, 0.4,0.34,'R (Pearson) : '+string(maxpbckgnd,format='(F10.4)'),color='red',/normal
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SHOWALLASTERS'):begin
      ntsize = size(node_table,/dimensions)
      numasters = ntsize[1]
      node_table,event,/pinpoint,selection=findgen(numasters), psym = 1,symsize=1,color='red',thick = 1, neighborhood = asterradius,/id
      
      pbratio = reform(node_table[5,*])
      if n_elements(pbratio) lt 50 then nbins = 10 else nbins = 25
      cghistoplot,pbratio,/noerase,binsize=0.25,axiscolorname='red',min_value=1.,/nan,/fillpolygon, position=[0.75,0.75,0.95,0.95]
      
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_CLEANUP'):node_table,event,cleanup=1.1
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_BATCH'):node_table_array_analysis, event,/pooled
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_BATCH2'): node_table_array_analysis, event,/bycells
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXPORTPOOLED'):node_table_array_analysis, event,/pooled,/export
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXPORTSPATIAL'): node_table_array_analysis, event,/edge,/export
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EXPORTSPATIAL2'): node_table_array_analysis, event,/area,/export
     
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SUBPIXELALL'): begin
      node_table, event, /subpixel
      node_table, event, /morphology,maskimage=maskimage
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SUBPIXEL'):begin
      selectedrow = (widget_info(widget_info(event.top,find_by_uname='WID_TABLE_NODES'),/table_select))[1,*]
      sel = selectedrow(uniq(selectedrow))
      if n_elements(image_stack) lt 1 then return
      if n_elements(node_table) lt 1 then return
      thisimage = reform(image_stack[*,*,frame-1])
      imsize = size(thisimage,/dimension)
      xdim = imsize[0] & ydim = imsize[1]
      thisx = node_table[1,sel[0]]
      thisy = node_table[2,sel[0]]
      xmin = (thisx-asterradius)>0
      ymin = (thisy-asterradius)>0
      xmax = (thisx+asterradius)<(xdim-1)
      ymax = (thisy+asterradius)<(ydim-1)
      subcrop = thisimage[xmin:xmax,ymin:ymax]-darkcount
      nq1 = node_subpixel(node_image=subcrop,x=thisx,y=thisy, results=results,/tilt,/verbose) 
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SHAPE'):begin
       selectedrow = (widget_info(widget_info(event.top,find_by_uname='WID_TABLE_NODES'),/table_select))[1,*]
       sel = selectedrow(uniq(selectedrow))
       if n_elements(image_stack) lt 1 then return
       if n_elements(node_table) lt 1 then return
       thisimage = reform(image_stack[*,*,frame-1])
       imsize = size(thisimage,/dimension)
       xdim = imsize[0] & ydim = imsize[1]
       thisx = node_table[1,sel[0]]
       thisy = node_table[2,sel[0]]
       xmin = (thisx-asterradius)>0
       ymin = (thisy-asterradius)>0
       xmax = (thisx+asterradius)<(xdim-1)
       ymax = (thisy+asterradius)<(ydim-1)
       subcrop = thisimage[xmin:xmax,ymin:ymax]-darkcount
       nq1 = node_morphology(node_image=subcrop,threshold=threshold,/verbose) 
       
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_SEGMENTATION'):begin
       if size(image_stack,/type) eq 0 then return
       imageraw = image_stack[*,*,frame-1]
       node_mask, event, mask=oldmask, get_mask=frame
       mask = aster_segment(imageraw,groupleader = wWidget, /interactive,default=oldmask)
       help, mask
       node_mask, event, mask=mask, set_mask=frame
     end     
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_EDITSEGMENTATION'):begin
       if size(image_stack,/type) eq 0 then return
       imageraw = image_stack[*,*,frame-1]
       node_mask, event,get_mask=frame, mask=mask
       help, mask
       newmask = aster_segment_edit(mask, groupleader=wWidget, grayscale = imageraw)
       node_mask, event, mask=newmask, set_mask=frame
     end     
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_RESETSEGMENTATION'):begin
      node_mask, event,get_mask=frame, mask=mask
      node_mask, event,set_mask=frame, mask = 0*mask
     end
     Widget_Info(wWidget, FIND_BY_UNAME='WID_BUTTON_RESETSEGMENTATIONALL'):node_mask, event,/reset
     
    else:
  endcase  
end

pro AsterAnalyzer, GROUP_LEADER=wGroup, _EXTRA=_VWBExtra_, image=im,modal=modal
common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array

  wxsz = 1024
  wysz = 1024
  
  xoffset=0.
  yoffset=0.
  colordroplist=['Red Temperature # 3','Grayscale # 0','Rainbow # 13','Inverse Grayscale','Rainbow #39','Inverse Blue']
  filtrackzoom = [0,0,1023,1023]
  displaylist = ['Raw','Mask','Overlay']
  detectionmodelist = ['Use Mask if available','Entire image']
  
  WID_BASE_node = Widget_Base( GROUP_LEADER=wGroup,  $
    UNAME='WID_BASE_NODE' ,XOFFSET=0 ,YOFFSET=0  $
    ,SCR_XSIZE=wxsz+600 ,SCR_YSIZE=wysz+90  $
    ,TITLE='Aster Analyzer release 1.0 (c) 2016-9 Kanchanawong Lab, MBI, NUS '+ $
    ' ' ,SPACE=3 ,XPAD=3 ,YPAD=3,modal=modal, notify_realize='initializenode')
    
  WID_IQUANT_DRAW = Widget_Draw(WID_BASE_node,  $
    UNAME='WID_DRAW_MAIN' ,FRAME=1 ,XOFFSET=5 ,YOFFSET=5  $
    ,SCR_XSIZE=wxsz ,SCR_YSIZE=wysz  $
    ,/BUTTON_EVENTS)
  
  WID_slider_frame =   Widget_Slider(WID_BASE_node,  $
    UNAME='WID_SLIDER_FRAME' ,XOFFSET=5 ,YOFFSET=wysz+5  $
    ,SCR_XSIZE=1000 ,SCR_YSIZE=40 ,TITLE='',MAXIMUM=100,value = 0)
    
  WID_TAB_MAIN = WIDGET_TAB( WID_BASE_node, /ALIGN_right , LOCATION=0, SCR_XSIZE=570, SCR_YSIZE=1000, XOFFSET=wxsz+15, XSIZE=570, YOFFSET=10, YSIZE=1000)
  wID_BASE_tab1 = WIDGET_BASE(WID_TAB_MAIN, TITLE='Main')
  wID_BASE_tab3 = WIDGET_BASE(WID_TAB_MAIN, TITLE='I/O ')
  wID_BASE_tab2 = WIDGET_BASE(WID_TAB_MAIN, TITLE='Asters Detection')
  wID_BASE_tab4 = WIDGET_BASE(WID_TAB_MAIN, TITLE='Asters Analysis')
  
  WID_BUTTON_EXIT = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_EXIT', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='EXIT', $
    Xoffset = 10, $
    Yoffset = 940)    
  WID_BUTTON_SAVESCREEN = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_SAVESCREEN', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Save Current Screen', $
    Xoffset = xoffset+10, $
    Yoffset = 940-35)

  WID_BUTTON_copyscreen = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_COPYSCREEN', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Copy Current Screen', $
    Xoffset = xoffset+10, $
    Yoffset = 940-35*2)
   
  WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_RESETZOOM', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Unzoomed', $
    Xoffset = xoffset+10, $
    Yoffset = 5)    
  WID_BUTTON_ZOOMOUT2X = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_ZOOMOUT2X', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Zoom Out 2X', $
    Xoffset = xoffset+10, $
    Yoffset = 5+35)   
  WID_BUTTON_ZOOMOUT2X = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_REFRESH', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Refresh', $
    Xoffset = xoffset+10, $
    Yoffset = 5+35*2)  
  WID_BUTTON_ZOOMOUT2X = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_CLEARSCREEN', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Clear Screen', $
    Xoffset = xoffset+10, $
    Yoffset = 5+35*3)   
    
    
    
  WID_BUTTON_ZOOMOUT2X = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_SEGMENTATION', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Define Analysis ROI', $
    Xoffset = xoffset+10, $
    Yoffset = 5+35*5)  
  WID_BUTTON_ZOOMOUT2X = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_EDITSEGMENTATION', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Edit Analysis ROI', $
    Xoffset = xoffset+10, $
    Yoffset = 5+35*6)  
  
  WID_BUTTON_ZOOMOUT2X = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_RESETSEGMENTATION', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Reset ROI, current frame', $
    Xoffset = xoffset+10, $
    Yoffset = 940-35*6)
  
  WID_BUTTON_ZOOMOUT2X = Widget_Button(wID_BASE_tab1, UNAME ='WID_BUTTON_RESETSEGMENTATIONALL', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Reset ROI, all frames', $
    Xoffset = xoffset+10, $
    Yoffset = 940-35*5)
    
    
  WID_DROPLIST_color = Widget_Droplist(wID_BASE_tab1,  $
    UNAME='WID_DROPLIST_COLOR' ,XOFFSET=xoffset+10+180+5+180+5,YOFFSET=5 ,SCR_XSIZE=180  $
    ,SCR_YSIZE=25 ,TITLE='Color :' ,VALUE=colordroplist)
    
  WID_DROPLIST_display = Widget_Droplist(wID_BASE_tab1,  $
    UNAME='WID_DROPLIST_DISPLAY' ,XOFFSET=xoffset+10+180+5+180+5,YOFFSET=5+35 ,SCR_XSIZE=180  $
    ,SCR_YSIZE=25 ,TITLE='Display :' ,VALUE=displaylist)  

  WID_DROPLIST_color = Widget_Droplist(wID_BASE_node,  $
    UNAME='WID_DROPLIST_MOUSE' ,XOFFSET=xoffset+1050,YOFFSET=1015 ,SCR_XSIZE=180  $
    ,SCR_YSIZE=25 ,TITLE='Mouse Mode :' ,VALUE=['Zoom','Selection','Add Aster'])  
    
    


  ;=========================I/O==============
  
  WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab3, UNAME ='WID_BUTTON_LOADTIFF', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Load *.tiff', $
    Xoffset = xoffset+10, $
    Yoffset = 5)   
    
  WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab3, UNAME ='WID_BUTTON_SAVESAV', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Save analysis to *asters.sav', $
    Xoffset = xoffset+10, $
    Yoffset = 5+35*2)   
 
  WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab3, UNAME ='WID_BUTTON_LOADSAV', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Load analysis from *asters.sav', $
    Xoffset = xoffset+10, $
    Yoffset = 5+35)   
  
  ;======
  WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab2, UNAME ='WID_BUTTON_DETECT', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Detect Asters', $
    Xoffset = xoffset+10, $
    Yoffset = 5+30+5)
  
  WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab2, UNAME ='WID_BUTTON_AUTOPARAM', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Auto. Parameters (PSF)', $
    Xoffset = xoffset+10, $
    Yoffset = 5)
  
  WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab2, UNAME ='WID_BUTTON_DETECTALL', $
    SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
    Value ='Detect Asters, All Images ', $
    Xoffset = xoffset+10, $
    Yoffset = yoffset+940)
  
  WID_DROPLIST_color = Widget_Droplist(wID_BASE_tab2,  $
    UNAME='WID_DROPLIST_DETECTION' ,XOFFSET=xoffset+10,YOFFSET=5+35*2 ,SCR_XSIZE=180  $
    ,SCR_YSIZE=25 ,TITLE='Mode:' ,VALUE=detectionmodelist)


    
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250, yoffset = 5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=0.01, maximum=20.,scroll = 0.1, $
    title='Gaussian Sigma (pixel)',value = 3.,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_GAUSSIANSIGMA')
      
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250, yoffset = 5+80+5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=3, maximum=20.,scroll = 0.1, $
    title='Window Radius (pixel)',value = 3.,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_WINDOWRADIUS')
    
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250, yoffset = 5+80*2+5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=5, maximum=1200.,scroll = 0.1, $
    title='Maximum Asters # to detect',value = 20.,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_MAXPEAKS')
    
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250, yoffset = 5+80*3+5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=400, maximum=1200.,scroll = 0.1, $
    title='Emission Wavelength (nm)',value = 520.,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_WAVELENGTH')
      
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250, yoffset = 5+80*4+5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=1., maximum=30.,scroll = 0.1, $
    title='Camera Pixel Size (um)',value = 6.5,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_CAMERAPIXELMICRON')
    
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250, yoffset = 5+80*5+5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=1., maximum=300.,scroll = 0.1, $
    title='Magnification (X)',value = 60.,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_MAGNIFICATION') 
    
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250, yoffset = 5+80*6+5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=0.2, maximum=2.,scroll = 0.01, $
    title='Numerical Aperture (N.A.)',value = 1.49,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_NA')     
    
    
  wid_base_imin = widget_base(wID_BASE_tab2, xoffset=5+250+5, yoffset = 5+80*7+5, scr_xsize = 250, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=0, maximum=2000,scroll = 1, $
    title='Camera Dark Offset (gray level)',value = 100,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_DARKCOUNT')
    

    ;================
    WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_BATCH', $
      SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
      Value ='Statistics, Pooled All Cells', $
      Xoffset = xoffset+10, $
      Yoffset = yoffset+940)
  
    WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_BATCH2', $
      SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
      Value ='Statistics, Cell-by-cell', $
      Xoffset = xoffset+10+180+5, $
      Yoffset = yoffset+940)
      
    WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_EXPORTPOOLED', $
      SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
      Value ='Export Pooled Statistics', $
      Xoffset = xoffset+10+180*2+5+5, $
      Yoffset = yoffset+940)   
    
    WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_EXPORTSPATIAL', $
      SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
      Value ='Export Inter-Aster Dist.', $
      Xoffset = xoffset+10+180*2+5+5, $
      Yoffset = yoffset+940-35)  
    
;    WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_EXPORTSPATIAL2', $
;      SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
;      Value ='Export Voronoi Area.', $
;      Xoffset = xoffset+10+180*2+5+5, $
;      Yoffset = yoffset+940-35*2)
  
  
    wid_base_imin = widget_base(wID_BASE_tab4, xoffset=5+250+5, yoffset = 5, scr_xsize = 250, scr_ysize=80)
    wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=1, maximum=50,scroll = 1, $
      title='Aster Neighborhood radius (pixel)',value = 6.,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_ASTERRADIUS')

    WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_SHOWALLASTERS', $
      SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
      Value ='Plot All Asters', $
      Xoffset = xoffset+10, $
      Yoffset = 5+35)  
    
    WID_BUTTON_ResetZoom = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_REFRESH2', $
      SCr_XSIZE = 180, SCR_YSIZE = 30,/Align_center,$
      Value ='Refresh', $
      Xoffset = xoffset+10, $
      Yoffset = 5)
    
  WID_Table_nodes= Widget_Table(WID_BASE_tab4,  $
    UNAME='WID_TABLE_NODES' ,xoffset = 25, YOFFSET=5+80+10 ,SCR_XSIZE=500  $
    ,SCR_YSIZE=250 ,COLUMN_LABELS=[ 'ID', 'X',  $
    'Y', 'MaxPeakVal.', 'MeanBkgrd.', 'meanP/B','MeanPeakVal','BkgrdSigma','BkgrndThres','maxP/B','X-fit','Y-fit','sigmaX','sigmaY','Amplitude','Offset','Area','Area-Intensity','Ellipticity'] ,$
    XSIZE=19 ,YSIZE=1000,column_widths=replicate(60, 19),/disjoint_selection,value=tablearray,format='(F12.3)')   
   
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_PINPOINT', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Locate Asters', $
    Xoffset = xoffset+10, $
    Yoffset = 5+80*1+10+250+5)
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_ZOOMNODE', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Aster Properties', $
    Xoffset = xoffset+10+100+5, $
    Yoffset = 5+80*1+10+250+5)
      
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_REMOVENODE', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Remove Asters', $
    Xoffset = xoffset+10+105*2+5, $
    Yoffset = 5+80*1+10+250+5)  
    
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_REMOVENODEID', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Remove by ID:', $
    Xoffset = xoffset+10+105*2+5, $
    Yoffset = 5+80*1+10+250+5+35)
   
  WID_text_id= Widget_text(wID_BASE_tab4, UNAME ='WID_TEXT_ASTERID', $
    SCr_XSIZE = 200, SCR_YSIZE = 30,/Align_left,$
    Value =[''], $
    Xoffset = xoffset+10+105*3+5, $
    Yoffset = 5+80*1+10+250+5+35,/WRAP ,XSIZE=20 ,YSIZE=1,/editable)
  
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_REFRESHTABLE', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Refresh Table', $
    Xoffset = xoffset+10+105*3+5, $
    Yoffset = 5+80*1+10+250+5)
    
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_EXPORTTABLE', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Export Table', $
    Xoffset = xoffset+10+105*4+5, $
    Yoffset = 5+80*1+10+250+5)
    
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_SORTTABLE', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Sort Table', $
    Xoffset = xoffset+10, $
    Yoffset = 5+80*1+10+250+5+35)   
    
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_CLEANUP', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Reject Bad Asters', $
    Xoffset = xoffset+10+105, $
    Yoffset = 5+80*1+10+250+5+35)  

  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_HISTOGRAMCOLUMN', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Histogram/Col.', $
    Xoffset = xoffset+10, $
    Yoffset = 5+80*1+10+250+5+35*2)
    
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_FULLSTAT1', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Statistics I', $
    Xoffset = xoffset+10+105, $
    Yoffset = 5+80*1+10+250+5+35*2)
    
;  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_FULLSTAT4', $
;    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
;    Value ='Statistics II', $
;    Xoffset = xoffset+10+105*2+5, $
;    Yoffset = 5+80*1+10+250+5+35*2)
   
;   WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_FULLSTAT2', $
;    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
;    Value ='Spatial Analysis', $
;    Xoffset = xoffset+10+105*2+5, $
;    Yoffset = 5+80*1+10+250+5+35*3) 
;    
;  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_FULLSTAT3', $
;    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
;    Value ='Diagnostics', $
;    Xoffset = xoffset+10+105*3+5, $
;    Yoffset = 5+80*1+10+250+5+35*2)     
;    
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_SUBPIXEL', $
    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
    Value ='Sub-pixel Fit', $
    Xoffset = xoffset+10+105*2+5, $
    Yoffset = 5+80*1+10+250+5+35*2)    
  
;  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_SHAPE', $
;    SCr_XSIZE = 100, SCR_YSIZE = 30,/Align_center,$
;    Value ='Shape Analysis', $
;    Xoffset = xoffset+10+105, $
;    Yoffset = 5+80*1+10+250+5+35*3)  
    
  wid_base_imin = widget_base(wID_BASE_tab4, xoffset=xoffset+10+105*3+5, yoffset = 5+80*1+10+250+5+35*3, scr_xsize = 220, scr_ysize=80)
  wid_slider_hmin = cw_fslider(wid_base_imin, /DOUBLE,minimum=1, maximum=20000,scroll = 1, $
    title='Global threshold (gray value)',value = 1000.,xsize=250,ysize = 65,/edit, uname='WID_SLIDER_THRESHOLD')  
  
  WID_BUTTON_Pinpoint = Widget_Button(wID_BASE_tab4, UNAME ='WID_BUTTON_SUBPIXELALL', $
    SCr_XSIZE = 210, SCR_YSIZE = 30,/Align_center,$
    Value ='Sub-pixel/Shape(All)', $
    Xoffset = xoffset+10+105*3+5, $
    Yoffset = 5+80*1+10+250+5+35*2)
    
  Widget_Control, /REALIZE, WID_BASE_node
  XManager, 'WID_BASE_NODE', WID_BASE_node,/NO_BLOCK
  
end

pro drawevents, event,frame=frame
common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array

  ;width = (size(filtrackImageraw,/dimensions))[0]
  ;height = (size(filtrackImageraw,/dimensions))[1]
  mousemode = widget_info(Widget_Info(event.top, FIND_BY_UNAME='WID_DROPLIST_MOUSE'),/droplist_select)
  if ~keyword_set(frame) then frame = 0
  width=properties.xpixels
  height=properties.ypixels
  mgw = wxsz/width
  drawwindow = !d.window
  
  IF event.type GT 2 THEN RETURN
  eventTypes = ['DOWN', 'UP', 'MOTION']
  thisEvent = eventTypes[event.type]
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  z = node_getzoomfactor()
  
;  xmin = frontendzoom[0] & ymin = frontendzoom[1]
;  xmax = frontendzoom[2] & ymax = frontendzoom[3]
;  xdim = xmax-xmin+1 & ydim = ymax-ymin+1
;  mgw = ((wxsz*1./xdim)<(wysz*1./ydim))

  if size(info,/type) ne 8 then begin
    info = infoinitial
    widget_control, event.top, set_uvalue=info,/no_copy
    print, 'Reinitialized info..'
    return
  end
  
  case thisevent of
    'DOWN':begin
    widget_control, info.drawID, draw_motion_events =1
    window, /free,/pixmap,xsize=info.xsize,ysize=info.ysize
    info.pixID = !D.window
    device,copy=[0,0,info.xsize,info.ysize,0,0,info.wid]
    info.sx = event.x >0
    info.sy = event.y >0
    wset, def_w
    mousedown = 1
    widget_control, event.top, set_uvalue=info,/no_copy
    ;print, 'down'
  end
  'MOTION': begin
    wset,info.wid
    device,copy=[0,0,info.xsize,info.ysize,0,0,info.pixID]
    sx=info.sx
    sy=info.sy
    dx=(event.x < wxsz)>0
    dy= (event.y < wysz)>0
    ; forcing square
    dim = abs(dx-sx)<abs(dy-sy)
    dx = (event.x gt sx) ? info.sx+dim:info.sx-dim
    dy = (event.y gt sy) ? info.sy+dim:info.sy-dim
    plotS, [sx,sx,dx,dx,sx],[sy,dy,dy,sy,sy],/device, color =info.boxcolor
    wset, def_w
    widget_control, event.top, set_uvalue=info,/no_copy
  end
  'UP': begin
    if mousedown eq 0 then begin
      widget_control, event.top, set_uvalue=info,/no_copy
      return
    endif
    ;      if size(info,/type) ne 8 then return
    mousedown = 0
    if info.pixID eq -1 then begin
      window, /free,/pixmap,xsize=info.xsize,ysize=info.ysize
      info.pixID = !D.window
      widget_control, event.top, set_uvalue=info,/no_copy
      return
    end
    device, window_state= windowstate
    if windowstate[info.pixID] lt 1 then return
    wset, info.wid
    
    device,copy=[0,0,info.xsize,info.ysize,0,0,info.pixID]
    wdelete, info.pixID
    widget_control, info.drawID, draw_motion_events =0, clear_events=1
    dx=(event.x < wxsz)>0
    dy= (event.y < wysz)>0
    sx=info.sx
    sy=info.sy
    ; forcing square
    dim = abs(dx-sx)<abs(dy-sy)
    dx = (event.x gt sx) ? info.sx+dim-1:info.sx-dim-1
    dy = (event.y gt sy) ? info.sy+dim-1:info.sy-dim-1
    
    sx= min([info.sx,dx],max=dx)
    sy= min([info.sy,dy],max=dy)
    
    oldfrontendzoom = frontendzoom
    newfrontendzoom = round([sx/z.mgw+z.xmin,sy/z.mgw+z.ymin,dx/z.mgw+z.xmin,dy/z.mgw+z.ymin])
    
     if (abs(newfrontendzoom[0]-newfrontendzoom[2]) gt 1 ) and (abs(newfrontendzoom[1]-newfrontendzoom[3]) gt 1) then begin
      frontendzoom= newfrontendzoom
      case mousemode of
        1: begin
          node_display, event, /nodeselection
          frontendzoom=oldfrontendzoom
        end
        2:begin
          addx = (newfrontendzoom[0]+newfrontendzoom[2])*0.5
          addy = (newfrontendzoom[1]+newfrontendzoom[3])*0.5
          frontendzoom=oldfrontendzoom
          print, 'Add X:',addx,' Y :',addy
          node_table,event,append=[addx,addy]
          frontendzoom=oldfrontendzoom
        end
        else:node_display,event,frame=frame
      endcase  
        ;function-display here
      
;      case widget_info(widget_info(event.top,find_by_uname='WID_DROPLIST_MODE'),/droplist_select) of
;        1:filamentdisplay, event, /enhanced, zoom=filtrackzoom
;        2:filamentdisplay, event, /original, /skeletonoverlay, zoom=filtrackzoom
;        3:filamentdisplay, event, /enhanced, /maskoverlay, zoom=filtrackzoom
;        4:filamentdisplay, event, /enhanced, /skeletonoverlay, zoom=filtrackzoom
;        else: filamentdisplay, event, /original, zoom=filtrackzoom
;      endcase      
     endif  
    
    widget_control, event.top, set_uvalue=info,/no_copy    
  end
endcase
 
 
end




function node_morphology,node_image=node_image, threshold=threshold, verbose=verbose
 peakindex = where(node_image ge threshold, count)
 if count lt 1 then return, -1
 immax = max(node_image,maxindex)
 peakx = (array_indices(node_image,maxindex))[0]
 peaky = (array_indices(node_image,maxindex))[1]
 inbound = search2d(node_image, peakx, peaky, threshold,immax+1)
 mask = node_image*0
 mask[inbound] = 1
 if total(mask) gt 1 then begin
  obj = Obj_New("BLOB_ANALYZER", mask,mask=mask)
  stats = obj -> fitellipse(0, center=center, axes=axes, orientation = orientation)
  if n_elements(axes) ne 2 then return, -1
  if keyword_set(verbose) then begin
    cgimage,node_image,/noerase,/scale,position=[0.,0.95,0.05,1.0]
    cgimage,mask,/noerase,/scale,position=[0.05,0.95,0.1,1.0]
    cgtext, 0.16,0.97,'Area:'+string(count,format='(F10.3)')+' ellipticity:'+string(max(axes)/min(axes),format='(F10.3)'),color='red',/normal
    ;print, count
  endif
  return, {mask:mask, area:count,axismajor:max(axes),axisminor:min(axes), ellipticity: max(axes)/min(axes), peakintensity: total(node_image[inbound])}
 endif else return, -1
 
 end

function node_subpixel,node_image=node_image,x=x,y=y,results=results,unrestrained=unrestrained, tilt=tilt,verbose=verbose
if n_elements(node_image) lt 1 then return, -1
imdim = size(node_image,/dimension)
xdim = imdim[0]
ydim = imdim[1]
if xdim ne ydim then return,-1
if xdim mod 2 eq 0 then return, -1 ; only work on odd-size array
offset = fix((xdim-1)/2)
xoffset = x-offset
yoffset = y-offset
xx = xoffset+findgen(xdim)
yy = yoffset+findgen(ydim)

Result = GAUSS2DFIT( node_image, A , Xx, Yy, tilt=tilt)

if keyword_set(unrestrained) then begin
  return, {x: a[4], y:a[5], sigmax: a[2], sigmay:a[3], amplitude:a[1],offset:a[0]}
endif else begin
  if sqrt(((a[4]-x)^2+(a[5]-y)^2)) gt 4 then begin
    cgimage,node_image,/noerase,/scale,position=[0.,0.95,0.05,1.0]
    cgimage,Result,/noerase,/scale,position=[0.05,0.95,0.1,1.0]
    cgimage,node_image-Result,/noerase,/scale,position=[0.1,0.95,0.15,1.0]
    cgtext, 0.16,0.97,'X-fit:'+string(a[4],format='(F10.3)')+' Y-fit:'+string(a[4],format='(F10.3)'),color='red',/normal
    cgtext, 0.16,0.95,'sigmaX:'+string(a[2],format='(F10.3)')+' sigmaY:'+string(a[3],format='(F10.3)'),color='red',/normal
    print, 'Fit-offset too large:', sqrt(((a[4]-x)^2+(a[5]-y)^2)), ' X,Y-fit:',a[4],a[5]
    return,-1 
  endif else begin
    if keyword_set(verbose) then begin
      cgimage,node_image,/noerase,/scale,position=[0.,0.95,0.05,1.0]
      cgimage,Result,/noerase,/scale,position=[0.05,0.95,0.1,1.0]
      cgimage,node_image-Result,/noerase,/scale,position=[0.1,0.95,0.15,1.0]
      cgtext, 0.16,0.97,'X-fit:'+string(a[4],format='(F10.3)')+' Y-fit:'+string(a[4],format='(F10.3)'),color='red',/normal
      cgtext, 0.16,0.95,'sigmaX:'+string(a[2],format='(F10.3)')+' sigmaY:'+string(a[3],format='(F10.3)'),color='red',/normal
    endif
    return, {x: a[4], y:a[5], sigmax: a[2], sigmay:a[3], amplitude:a[1],offset:a[0]}
  endelse
endelse

end

function node_quantify, node_image=node_image, ratio = ratio, gaussian=gaussian, radius=radius,darklevel=darklevel

if ~keyword_set(darklevel) then darklevel = 0.
if n_elements(node_image) lt 1 then return, -1
imdim = size(node_image,/dimension)
xdim = imdim[0]
ydim = imdim[1]

if keyword_set(ratio) then begin
  mask = getCircularMask(node_image,radius)
  backgroundindex = where(mask eq 0, numbackgroundpix)
  nodeindex = where(mask, numnodepix)
  avgnodeintensity = mean(node_image[nodeindex]-darklevel,/nan)
  avgbackgroundintensity = mean(node_image[backgroundindex]-darklevel,/nan)
  backgroundsigma = stddev(node_image[backgroundindex]-darklevel,/nan)
  backgroundthres = backgroundsigma+avgbackgroundintensity
  nodepeakintensity = max(node_image[backgroundindex]-darklevel)
  nodepeakratio = max(node_image[nodeindex]-darklevel)/avgbackgroundintensity
  return, {nodeintensityratio:avgnodeintensity/avgbackgroundintensity, nodemeanintensity:avgnodeintensity,nodepeakratio: nodepeakratio, $
    backgroundmeanintensity:avgbackgroundintensity, nodepeakintensity: nodepeakintensity, backgroundsigma: backgroundsigma,backgroundthreshold:backgroundthres }
  
endif

if keyword_set(gaussian) then begin
  
  
endif

end

pro node_quantify_spatial, node_table=node_table,color=color,thick=thick, properties=properties, verbose=verbose, distance=edgeslength,area=voronoiarea
  if n_elements(node_table) eq 0 then return
  if ~keyword_set(color) then color = 'red'
  if ~keyword_set(thick) then thick = 2
  xx = reform(node_table[1,*])
  yy = reform(node_table[2,*])
  numpoints = n_elements(xx)
  
  TRIANGULATE, Xx, Yy, Triangles, hullcoord, CONNECTIVITY=connect
  numtri = (size(triangles,/dimensions))[1]
  trisize = dblarr(numtri)
  xvertex = dblarr(3,numtri)
  yvertex = dblarr(3,numtri)
  xvertex = [xx[triangles[0,*]],xx[triangles[1,*]],xx[triangles[2,*]]]
  yvertex = [yy[triangles[0,*]],yy[triangles[1,*]],yy[triangles[2,*]]]
  trisize = reform(0.5*abs(xvertex[0,*]*(yvertex[1,*]-yvertex[2,*])+xvertex[1,*]*(yvertex[2,*]-yvertex[0,*])+xvertex[2,*]*(yvertex[0,*]-yvertex[1,*])))
  normtrisize = 255*(1-trisize/max(trisize))^2

 
  z= node_getzoomfactor()
  xv= (xvertex-z.xmin)*z.mgw
  yv= (yvertex-z.ymin)*z.mgw

    if keyword_set(verbose) then  for i = 0, numtri-1 do cgplots, xv[*,i],yv[*,i],/device,color = color,thick=thick
   
   xhull = reform(node_table[1,hullcoord]) 
   yhull = reform(node_table[2,hullcoord]) 
    
   xhullplot = ([xhull, xhull[0]]-z.xmin)*z.mgw
   yhullplot = ([yhull, yhull[0]]-z.ymin)*z.mgw
   if keyword_set(verbose) then  cgplots, xhullplot,yhullplot,/device, color='yellow',thick=3
   
   numedges = 3*numpoints-3-n_elements(hullcoord)
   print,'Number of Points: ', numpoints, ' Number of Edges:', numedges, ' Number of Hull-vertices:', n_elements(hullcoord)

  distancematrix = dblarr(numpoints,numpoints)+!values.f_nan
  for i = 0, numpoints-1 do begin
    neighborindex = connect[connect[i]:connect[i+1]-1]
    numneighbor = n_elements(neighborindex)
    thisx = node_table[1,i]
    thisy = node_table[2,i]
    for j = 0, numneighbor-1 do       distancematrix[i,neighborindex[j]] = sqrt((thisx-node_table[1,neighborindex[j]])^2+(thisy-node_table[2,neighborindex[j]])^2)
  endfor
  
  for i = 0, numpoints-1 do distancematrix[i,i] = !values.f_nan
   
  uniqueedge = distancematrix*uppertriangle(numpoints,/nan)
  indexuniqueedge = where(finite(distancematrix*uppertriangle(numpoints,/nan)),uniqueedges)
  print, 'Unique edges:', uniqueedges
  edgeslength = reform(distancematrix[indexuniqueedge],uniqueedges)

  if keyword_set(verbose) then cghistoplot, distancematrix[indexuniqueedge],/noerase,axiscolor='red',backcolor='white',/fillpolygon,nbins=25, position=[0.1,0.1,0.3,0.3],xtitle='inter-aster-distance(pix)'
  
  voronoiarea = dblarr(numpoints)
  for i = 0, numpoints-1 do begin
    VORONOI, xx, yy, i, Connect, Xp, Yp,[0,0,properties.xpixels,properties.ypixels]
    voronoiarea[i] = areapolygon(xp,yp)  
     if keyword_set(verbose) then cgpolygon, z.mgw*(Xp-z.xmin), z.mgw*(Yp-z.ymin),/device, COLOR = 'gray'
    ;print,    voronoiarea[i]  
  endfor
  
  hullarea = areapolygon(reform(node_table[1,hullcoord]),reform(node_table[2,hullcoord]))
  print, 'Hull area', hullarea
  voronoiarea = voronoiarea[where((voronoiarea gt 0) and (voronoiarea lt hullarea))]
   if keyword_set(verbose) then cghistoplot, voronoiarea,/noerase,axiscolor='red',backcolor='white',/fillpolygon,min_value=0,binsize=50,maxinput=1000, position=[0.4,0.1,0.7,0.3],xtitle='Aster-voronoi-area(pix^2)',/nan
 
 edgeslength = edgeslength
end

pro node_table_array_analysis, event,aggregate_node_table=aggregate_node_table,count=aggregate_rows, bycells=bycells, pooled=pooled, export=export, area=area, edge=edge
common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array

if n_elements(node_table_array) eq 0 then return
numcells = n_elements(node_table_array)
if total(ptr_valid(node_table_array)) lt 1 then return
wherevalid = where(ptr_valid(node_table_array), numvalid)
thisnode_table=*(node_table_array[wherevalid[0]])
tabdim = size(thisnode_table,/dimensions)
numrows =tabdim[1]
aggregate_node_table = dblarr(20, numrows)
aggregate_node_table[0,*]=replicate(1+wherevalid[0], numrows)
aggregate_node_table[1:19,*]=thisnode_table
aggregate_rows = numrows
node_quantify_spatial,node_table=thisnode_table, properties=properties, distance=edgeslength,area=voronoiarea
aggregrate_voronoiarea = voronoiarea & aggregate_distance = edgeslength
aggregrate_voronoiarea_cidex = voronoiarea*0.+(1+wherevalid[0])
aggregate_distance_cidex = edgeslength*0.+(1+wherevalid[0])

ptrarr_voronoiarea  = ptrarr(numvalid) &ptrarr_mxpb=ptrarr(numvalid) &ptrarr_distance=ptrarr(numvalid)

ptrarr_voronoiarea[0]  = ptr_new(voronoiarea)
ptrarr_mxpb[0]=ptr_new(reform(thisnode_table[9,*]))
ptrarr_distance[0]=ptr_new(edgeslength)
for i = 1, numvalid-1 do begin
  if ptr_valid(node_table_array[wherevalid[i]]) then begin
    thisnode_table=*(node_table_array[wherevalid[i]])
    tabdim = size(thisnode_table,/dimensions)
    numrows =tabdim[1]
    newtable = dblarr(20, aggregate_rows+numrows)
    newtable[*,0:aggregate_rows-1] = aggregate_node_table
    newtable[0,aggregate_rows:aggregate_rows+numrows-1] = replicate(1+ wherevalid[i], numrows)

    newtable[1:19,aggregate_rows:aggregate_rows+numrows-1]=thisnode_table
    aggregate_node_table = newtable
    aggregate_rows+=numrows
    
    node_quantify_spatial,node_table=thisnode_table, properties=properties, distance=edgeslength,area=voronoiarea
    aggregrate_voronoiarea = [aggregrate_voronoiarea, reform(voronoiarea)]
    aggregate_distance = [aggregate_distance, reform(edgeslength)]
    aggregrate_voronoiarea_cidex = [aggregrate_voronoiarea_cidex, reform(voronoiarea*0.+(1+wherevalid[i]))]
    aggregate_distance_cidex = [aggregate_distance_cidex,reform(edgeslength*0.+(1+wherevalid[i]))]
    
    ptrarr_voronoiarea[i]  = ptr_new(voronoiarea)
  
    ptrarr_mxpb[i]=ptr_new(reform(thisnode_table[9,*]))
    ptrarr_distance[i]=ptr_new(edgeslength)
  endif 
endfor

help, aggregate_node_table
help, aggregate_rows
help, aggregrate_voronoiarea
help, aggregate_distance

if keyword_set(bycells) then begin
  cgboxplot, ptrarr_mxpb,/noerase,axiscolor='red',xtitle='cells',ytitle='mxP/B ratio',position=[0.1,0.55,0.95,0.95],color='red',thick=2
  cgboxplot, ptrarr_distance,/noerase,axiscolor='red',xtitle='cells',ytitle='inter-asters distance(pix)',position=[0.1,0.08,0.45,0.45],color='red',thick=2
  cgboxplot, ptrarr_voronoiarea,/noerase,axiscolor='red',xtitle='cells',ytitle='voronoi area',position=[0.55,0.08,0.95,0.45],color='red',thick=2
  return
endif

if keyword_set(area) and keyword_set(export) then begin
  exportmatrix = dblarr(2,n_elements(aggregrate_voronoiarea))
  exportmatrix[1,*] = aggregrate_voronoiarea
  exportmatrix[0,*] = aggregrate_voronoiarea_cidex
  filename = Dialog_Pickfile(/write,get_path=fpath,filter=['*.csv','*.txt'],title='Save voronoi-rea of asters (pixels-sq) to comma-delimited file')
  if filename eq '' then return
  cd,fpath
  write_csv,filename,exportmatrix,table_header=['  cell#  ','  voronoi-area  . ']
  dummy = dialog_message('Saving completed...')
  return
endif

if keyword_set(edge) and keyword_set(export) then begin
    exportmatrix = dblarr(2,n_elements(aggregate_distance))
    exportmatrix[1,*] = aggregate_distance
    exportmatrix[0,*] = aggregate_distance_cidex
    filename = Dialog_Pickfile(/write,get_path=fpath,filter=['*.csv','*.txt'],title='Save inter-aster distance (pixels) to comma-delimited file')
    if filename eq '' then return
    cd,fpath
    write_csv,filename,exportmatrix,table_header=['  cell#  ','  inter-aster dist. ']
    dummy = dialog_message('Saving completed...') 
  return
endif

if keyword_set(pooled) then begin
  cgplot, aggregate_node_table[1+6,*],aggregate_node_table[1+3,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='meanPeak',ytitle='maxPeak',position=[0.075,0.05,0.315,0.32]
  peakr = correlate(aggregate_node_table[1+6,*],aggregate_node_table[1+3,*])
  cgtext, 0.1,0.34,'R (Pearson) : '+string(peakr,format='(F10.4)'),color='red',/normal
  cgplot, aggregate_node_table[1+4,*],aggregate_node_table[1+7,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='meanBkgnd',ytitle='BkgndSigma',position=[0.395,0.05,0.635,0.32]
  backr = correlate(aggregate_node_table[4,*],aggregate_node_table[7,*])
  cgtext, 0.4,0.34,'R (Pearson) : '+string(backr,format='(F10.4)'),color='red',/normal
  cgplot, aggregate_node_table[1+5,*],aggregate_node_table[1+9,*],/noerase,psym=1,xstyle=1,ystyle=1,color='red',background='white',axiscolor='red', xtitle='meanP/B',ytitle='maxP/B',position=[0.72,0.05,0.995,0.32]
  maxr = correlate(aggregate_node_table[1+5,*],aggregate_node_table[1+9,*])
  cgtext, 0.75,0.34,'R (Pearson) : '+string(maxr,format='(F10.4)'),color='red',/normal

  if n_elements(coltohisto) lt 50 then nbins = 10 else nbins = 25
  cghistoplot,aggregate_node_table[1+3,*],/noerase,nbins=nbins,axiscolorname='red',/nan, position=[0.075,0.68,0.315,0.87],xtitle='maxPeak',/fillpolygon
  cghistoplot,aggregate_node_table[1+4,*],/noerase,nbins=nbins,axiscolorname='red',/nan,position=[0.395,0.68,0.635,0.87],xtitle='meanBkgnd',/fillpolygon
  cghistoplot,aggregate_node_table[1+9,*],/noerase,nbins=nbins,axiscolorname='red',/nan, position=[0.72,0.68,0.995,0.87],xtitle='maxP/B',/fillpolygon
  cgtext, 0.1,0.96,'Rep. MaxPeak: '+string(mean(/nan,aggregate_node_table[1+3,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.4,0.96,'Rep. Bkgnd: '+string(mean(/nan,aggregate_node_table[1+4,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.75,0.96,'Rep. mxP/B: '+string(mean(/nan,aggregate_node_table[1+9,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.1,0.935,'Max. MaxPeak: '+string(max(/nan,aggregate_node_table[1+3,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.4,0.935,'Max. Bkgnd: '+string(max(/nan,aggregate_node_table[1+4,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.75,0.935,'Max. mxP/B: '+string(max(/nan,aggregate_node_table[1+9,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.1,0.91,'Min MaxPeak: '+string(min(/nan,aggregate_node_table[1+3,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.4,0.91,'Min Bkgnd: '+string(min(/nan,aggregate_node_table[1+4,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.75,0.91,'Min mxP/B): '+string(min(/nan,aggregate_node_table[1+9,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.1,0.885,'Std MaxPeak: '+string(stddev(/nan,aggregate_node_table[1+3,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.4,0.885,'Std Bkgnd: '+string(stddev(/nan,aggregate_node_table[1+4,*] ),format='(F10.4)'),color='red',/normal
  cgtext, 0.75,0.885,'Std mxP/B: '+string(stddev(/nan,aggregate_node_table[1+9,*] ),format='(F10.4)'),color='red',/normal
  
  cgtext, 0.05,0.975, '# Cells: '+string(numvalid,format='(I5)')+ ' # Asters:' + string(aggregate_rows,format='(I5)'),color='red',charsize = 2,/normal
    
  cghistoplot, aggregate_distance,/noerase,axiscolor='red',backcolor='white',/fillpolygon,nbins=25, position=[0.15,0.41,0.5,0.62],xtitle='inter-aster-distance(pix)'
  cghistoplot, aggregrate_voronoiarea,/noerase,axiscolor='red',backcolor='white',/fillpolygon,min_value=0,binsize=50,maxinput=1000, position=[0.6,0.41,0.95,0.62],xtitle='Aster-voronoi-area(pix^2)',/nan
  if keyword_set(export) then begin
     filename = Dialog_Pickfile(/write,get_path=fpath,filter=['*.csv','*.txt'],title='Save asters table to comma-delimited file')
    if filename eq '' then return
    cd,fpath
    tableheader = ['Cell-ID ',' Aster-ID ', ' X ',  $
      ' Y ', ' MaxPeakVal. ', ' MeanBkgrd. ', ' meanP/B ',' MeanPeakVal ',' BkgrdSigma ',' BkgrndThres ',' maxP/B ','n.a.','n.a.','n.a.','n.a.','n.a.','n.a.','n.a.']
    write_csv,filename,aggregate_node_table,table_header=tableheader
    dummy = dialog_message('Saving completed...')
    return
      return
  endif  

endif
end

pro node_table, event, update=update, refresh=refresh, pinpoint=pinpoint, delete=delete, selection=selection, zoom=zoom, psym=psym,symsize=symsize,id=id,color=color,thick=thick, $
  cleanup=cleanup, neighborhood=neighborhood, append=append, subpixel=subpixel, morphology=morphology, maskimage=maskimage,minpeakgray=minpeakgray
  common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
  common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array
  
  widget_control, widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),get_value=thisframe
  widget_control,Widget_Info(event.top, FIND_BY_UNAME='WID_SLIDER_ASTERRADIUS'),get_value=asterradius
  widget_control,Widget_Info(event.top, FIND_BY_UNAME='WID_SLIDER_DARKCOUNT'),get_value=darkcount
  asterradius = fix(asterradius)
  widget_control,Widget_Info(event.top, FIND_BY_UNAME='WID_SLIDER_GAUSSIANSIGMA'),get_value=gaussian_sigma
  widget_control,Widget_Info(event.top, FIND_BY_UNAME='WID_SLIDER_THRESHOLD'),get_value=threshold
  ;if ~keyword_set(neighborhood) then neighborhood = 6
  tabdim = size(node_Table,/dimensions)
  numrows =tabdim[1]
  frameindex = thisframe-1
  
  if keyword_set(minpeakgray) then begin
    if n_elements(node_table) eq 0 then return
    peak = reform(node_table[3,*])
    wherekeep = where(peak gt minpeakgray, numkeep,/null)    
    node_table=node_table[*,wherekeep]
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)
    return
  endif

 
  if keyword_set(cleanup) then begin
    if n_elements(node_table) eq 0 then return
    peakbkgndratio = reform(node_table[5,*])
    peak = reform(node_table[3,*])
    bkgnd  = reform(node_table[4,*])
    wherekeep = where((peak gt 0) and (bkgnd gt 0) and (peakbkgndratio gt cleanup), numkeep)
    node_table=node_table[*,wherekeep]
    pbindex =reform(node_table[5,*])
    node_table=node_table[*,reverse(sort(pbindex))]
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)
    return
  endif
  
  if keyword_set(update) then begin
    if n_elements(node_searchbuffer) eq 0 then return
    if size(node_searchbuffer,/type) ne 8 then return
    numpoints= node_searchbuffer.peakcount
    node_table = dblarr(19,numpoints) 
    node_table[0,*]=1+findgen(numpoints)
    node_table[1,*]=node_searchbuffer.x
    node_table[2,*]=node_searchbuffer.y
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)
    return
  endif
  
  if n_elements(image_stack) lt 1 then return
  thisimage = reform(image_stack[*,*,thisframe-1])
  imsize = size(thisimage,/dimension)
  xdim = imsize[0] & ydim = imsize[1]
  if ~keyword_set(selection) then selection = [0]
  
  if keyword_set(refresh) then begin
    if n_elements(node_searchbuffer) eq 0 then return
    if size(node_searchbuffer,/type) ne 8 then return
    numpoints= (size(node_table,/dimensions))[1];node_searchbuffer.peakcount
    for i = 0, numpoints-1 do begin
      thisx = node_table[1,i]
      thisy = node_table[2,i]
      xmin = (thisx-asterradius)>0
      ymin = (thisy-asterradius)>0
      xmax = (thisx+asterradius)<(xdim-1)
      ymax = (thisy+asterradius)<(ydim-1)
      subcrop = thisimage[xmin:xmax,ymin:ymax]-darkcount
      nq1 = node_quantify(node_image=subcrop, darklevel=darkcount,/ratio,radius=gaussian_sigma*2)
      node_table[3,i] = nq1.nodepeakintensity
      node_table[4,i] = nq1.backgroundmeanintensity
      node_table[5,i] = nq1.nodeintensityratio
      node_table[6,i] = nq1.nodemeanintensity
      node_table[7,i] = nq1.backgroundsigma
      node_table[8,i] = nq1.backgroundthreshold
      node_table[9,i] = nq1.nodepeakratio

    endfor
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)
    return
  endif
  
  if keyword_set(pinpoint) then begin
    if ~keyword_set(psym) then psym = 9
    if ~keyword_set(symsize) then symsize = 4
    if ~keyword_set(color) then color = 'green'
    if ~keyword_set(thick) then thick = 2
    z = node_getzoomfactor()
    
    for i = 0,n_elements(selection)-1 do begin
      cgplots,(node_table[1,selection[i]]-z.xmin)*z.mgw,(node_table[2,selection[i]]-z.ymin)*z.mgw,/device,psym=psym,symsize=symsize,color=color,thick = thick
      if keyword_set(neighborhood) then begin
        xxmin = node_table[1,selection[i]]-1*neighborhood
        xxmax = node_table[1,selection[i]]+1*neighborhood
        yymin = node_table[2,selection[i]]-1*neighborhood
        yymax = node_table[2,selection[i]]+1*neighborhood
        cgplots, [(xxmin-z.xmin)*z.mgw,(xxmax-z.xmin)*z.mgw ],[(yymin-z.ymin)*z.mgw,(yymin-z.ymin)*z.mgw ],color=color,/device
        cgplots, [(xxmax-z.xmin)*z.mgw,(xxmax-z.xmin)*z.mgw ],[(yymin-z.ymin)*z.mgw,(yymax-z.ymin)*z.mgw ],color=color,/device
        cgplots, [(xxmax-z.xmin)*z.mgw,(xxmin-z.xmin)*z.mgw ],[(yymax-z.ymin)*z.mgw,(yymax-z.ymin)*z.mgw ],color=color,/device
        cgplots, [(xxmin-z.xmin)*z.mgw,(xxmin-z.xmin)*z.mgw ],[(yymin-z.ymin)*z.mgw,(yymax-z.ymin)*z.mgw ],color=color,/device
      endif
      if keyword_set(id) then begin
        tposx = ((node_table[1,selection[i]]+0.5)-z.xmin)*z.mgw
        tposy = ((node_table[2,selection[i]]+0.5)-z.ymin)*z.mgw
        idtext = string(node_table[0,selection[i]],format='(I5)')
        cgtext,tposx,tposy,idtext,/device,color=color
      endif
      subx = node_table[10,selection[i]]
      suby = node_table[11,selection[i]]
      if subx*suby ne 0 then cgplots,(node_table[10,selection[i]]-z.xmin)*z.mgw,(node_table[11,selection[i]]-z.ymin)*z.mgw,/device,psym=1,symsize=1,color='red',thick = 1
    endfor
    return
  endif
  
  if keyword_set(morphology) then begin
    maskimage =thisimage*0
    numpoints= (size(node_table,/dimensions))[1]
    for i = 0, numpoints-1 do begin
      thisx = node_table[1,i]
      thisy = node_table[2,i]
      xmin = (thisx-asterradius)>0
      ymin = (thisy-asterradius)>0
      xmax = (thisx+asterradius)<(xdim-1)
      ymax = (thisy+asterradius)<(ydim-1)
      subcrop = thisimage[xmin:xmax,ymin:ymax]-darkcount
      nq1 = node_morphology(node_image=subcrop,threshold=threshold)
      if size(nq1,/type) eq 8 then begin
        node_table[18,i] = nq1.ellipticity
        node_table[17,i] = nq1.peakintensity
        node_table[16,i] = nq1.area   
        maskimage[xmin:xmax,ymin:ymax] = nq1.mask     
      endif  else node_table[16:18,i] = [0,0,0]      
    endfor
    tvscl, maskimage
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)
    return
  endif
  
  if keyword_set(subpixel) then begin
    numpoints= (size(node_table,/dimensions))[1]
    for i = 0, numpoints-1 do begin
      thisx = node_table[1,i]
      thisy = node_table[2,i]
      xmin = (thisx-asterradius)>0
      ymin = (thisy-asterradius)>0
      xmax = (thisx+asterradius)<(xdim-1)
      ymax = (thisy+asterradius)<(ydim-1)
      subcrop = thisimage[xmin:xmax,ymin:ymax]-darkcount
      nq1 = node_subpixel(node_image=subcrop,x=thisx,y=thisy, results=results,/tilt)
      if size(nq1,/type) eq 8 then begin
        node_table[10,i] = nq1.x
        node_table[11,i] = nq1.y
        node_table[12,i] = nq1.sigmax
        node_table[13,i] = nq1.sigmay
        node_table[14,i] = nq1.amplitude
        node_table[15,i] = nq1.offset
      endif  else node_table[10:15,i] = 0      
    endfor
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)    
    return
   endif

  
  if keyword_set(append) then begin
    if n_elements(append) ne 2 then return
    if n_elements(node_table) lt 1 then node_table = dblarr(19,1)
    idcurrent = node_table[0,*]
    maxid = max(idcurrent)
    numrows = n_elements(idcurrent)
    newnode_table = dblarr(19,numrows+1)
    newnode_table[*,0:numrows-1] = node_table
    newnode_table[0,numrows] = 1+maxid
    newnode_table[1,numrows]= append[0]
    newnode_table[2,numrows]=append[1]
    node_table=newnode_table
    node_table,event,/refresh
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)    
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_table_select=[-1,numrows]
    return
  endif
  
  if keyword_set(delete) and keyword_set(selection) then begin
    print, 'Remove by table selection'
    dellist = (selection>0)<(numrows-1)
    dellist =dellist[uniq(dellist[sort(dellist)])]
    keepindex = indgen(numrows)
    wherekeep = cgsetdifference(keepindex,dellist)
    node_table = node_table[*,wherekeep]
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)
    return
  endif
  
  if keyword_set(delete) and keyword_set(id) then begin
    print, 'Remove by id'
    allid = uint(reform(node_table[0,*]))
    ;print, allid
    dellist = cgsetintersection(allid,id,count=count,indices_a=deleteindex)
    if count lt 1 then begin
      print,'ID not found'
      return
    endif
    keepindex = indgen(numrows)
    wherekeep = cgsetdifference(keepindex,deleteindex)
    node_table = node_table[*,wherekeep]
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_value=node_table
    node_table_array[frameindex] = ptr_new(node_table)
    return
  endif
  
end

pro node_display, event, reset=reset, frame=frame, averaged = averaged, in2x=in2x, out2x=out2x, nodeselection=nodeselection, maskoverlay=maskoverlay, showmask=showmask
  common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
  common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array
  
  if size(properties, /type) eq 0 then return
  if size(image_stack, /type) eq 0 then return
  
  if keyword_set(nodeselection) then begin
    if n_elements(node_table) eq 0 then return
    z = node_getzoomfactor()
    thex = reform(node_table[1,*])
    they = reform(node_table[2,*])
    inbound = where( (thex ge z.xmin) and (thex le z.xmax) and (they ge z.ymin) and (they le z.ymax),countinbound)
    if countinbound lt 1 then return
    selection = replicate(-1, 2,countinbound)
    selection[1,*] = inbound
    widget_control, widget_info(event.top,find_by_uname='WID_TABLE_NODES'),set_table_select=selection
    return       
  endif
  
  
  if keyword_set(reset) then begin
    zoomcoord = [0,0,properties.xpixels-1,properties.ypixels-1]
    widget_control,widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),set_value=1
    widget_control,widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),set_slider_max=properties.frames
    widget_control,widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),set_slider_min=1  
    frontendzoom = [0,0, properties.xpixels,properties.ypixels]
    return  
  endif
  
  if keyword_set(in2x) then begin
    zoomold = node_getzoomfactor()
    newxmin = (zoomold.xmin+zoomold.xrange*0.25)>0
    newxmax = (zoomold.xmax-zoomold.xrange*0.25)<(properties.xpixels)
    newymin = (zoomold.ymin+zoomold.yrange*0.25)>0
    newymax = (zoomold.ymax-zoomold.yrange*0.25)<(properties.ypixels)
    frontendzoom = [newxmin,newymin, newxmax,newymax]
  endif
  
  if keyword_set(out2x) then begin
    zoomold = node_getzoomfactor()
    newxmin = (zoomold.xmin-zoomold.xrange*0.5)>0
    newxmax = (zoomold.xmax+zoomold.xrange*0.5)<(properties.xpixels)
    newymin = (zoomold.ymin-zoomold.yrange*0.5)>0
    newymax = (zoomold.ymax+zoomold.yrange*0.5)<(properties.ypixels)
    frontendzoom = [newxmin,newymin, newxmax,newymax]
  endif
  
  widget_control,widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),get_value=theframe
  theframe = theframe<(properties.frames)
  if keyword_set(frame) then begin
    theframe = frame
    widget_control,widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),set_value=theframe
  endif
  
  theframe=(theframe-1)>0
  z = node_getzoomfactor()
    
  if keyword_set(averaged) then begin
    cgimage, bytscl(congrid(mean(image_stack[z.xmin:z.xmax,z.ymin:z.ymax,*],dimension=3),z.mgw*z.xdim,z.mgw*z.ydim))
    return
  endif
  
  im = image_stack[z.xmin:z.xmax,z.ymin:z.ymax,theframe] 
  
  minim = min(im, max = maxim)
  maxim = maxim>(minim+1)

  imscl = congrid(im,z.mgw*z.xdim,z.mgw*z.ydim)
  
  if ~keyword_set(showmask) then cgimage, bytscl(imscl,min=minim,max=maxim),position=[0.,0.,1.,1.]
  
  if keyword_set(showmask) then begin
    node_mask, event, get_mask=frame,mask=mask
    maskzoom = mask[z.xmin:z.xmax,z.ymin:z.ymax]
    cgImage, bytscl(~maskzoom, min =0, max = 1),ctIndex = xt, position = [0,0,1,1.],/noerase
  endif
  
  if keyword_set(maskoverlay) then begin
     node_mask, event, get_mask=frame,mask=mask
     maskzoom = mask[z.xmin:z.xmax,z.ymin:z.ymax]
     cgImage, bytscl(~maskzoom, min =0, max = 1),ctIndex = xt, transparent = 70,position = [0,0,1,1.],/noerase    
  endif
end

pro node_mask, event, reset=reset, override=override, get_mask=get_mask, mask=mask, set_mask=set_mask, x=x, y=y,boundcheck=boundcheck, inbound=inbound, index1d = index1d, verbose =verbose
  common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array
  common node_mask, ImageROI_ptrarr_ptrarr
  
  if keyword_set(boundcheck) then begin
    nmask = ((boundcheck-1)>0)<(properties.frames-1)
    print, 'nmask:', nmask
    mask = *ImageROI_ptrarr_ptrarr[nmask]
;    help, mask
;    help, image_stack
    ;mask=*((*ImageROI_ptrarr_ptrarr[nmask])[0])  
    if keyword_set(verbose) then cgimage, mask,/noerase,/scale,position=[0.15,0,0.3,0.15]
    incoord = where(mask gt 0, ncount)
    inbound = 0
    if ncount lt 1 then inbound = 0
    inmaskdim = size(mask,/dimensions)        
    if keyword_set(index1d) then begin
      if mask[index1d] gt 0 then inbound =1
      print, 'index1d'
      return
    endif
    if ~keyword_set(index1d) then begin 
      if mask[round(x),round(y)] gt 0 then inbound = 1
      ;print, 'x:', x,' y:', y,' mask:',mask[round(x),round(y)] 
      return
    ENDIF
    RETURN
  endif
  
  if keyword_set(set_mask) then begin
    print,'Set_mask'
    nmask = ((set_mask-1)>0)<(properties.frames-1)
    inmaskdim = size(mask,/dimensions)
    if (inmaskdim[0] ne properties.xpixels) or (inmaskdim[1] ne properties.ypixels) then begin
      print, 'Incorrect dimension'
      return
    endif
    ImageROI_ptrarr_ptrarr[nmask] = [ptr_new(mask)]
;    help, mask
;    help, ImageROI_ptrarr_ptrarr
    return
  endif
  
  if keyword_set(get_mask) then begin
    nmask = ((get_mask-1)>0)<(properties.frames-1)
    mask = *ImageROI_ptrarr_ptrarr[nmask]
    ;mask = *(frameptrarr[0])
    ;mask=*((*ImageROI_ptrarr_ptrarr[nmask])[0])  
   ; help, mask  
    return
  endif
  
  if keyword_set(reset) then begin
    if ~keyword_set(override) then begin
      z= dialog_message('Reset all ROI?',/cancel)
      if Z eq 'Cancel' then return
    endif
    blank = bytarr(properties.xpixels,properties.ypixels)
    ImageROI_ptrarr_ptrarr=ptrarr(properties.frames)
    for i = 0, properties.frames-1 do ImageROI_ptrarr_ptrarr[i]=[ptr_new(blank)]   
    print,'Reset all mask'
    return
  endif
  
  
end

pro node_io, event, loadsav=loadsav, savesav=savesav
common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array
common node_mask, ImageROI_ptrarr_ptrarr

if keyword_set(savesav) then begin
  filename = Dialog_Pickfile(/write,get_path=fpath,filter=['*asters.sav'],title='Export clustering data into *asters.sav file')
  if filename eq '' then return
  cd,fpath
  filename=AddExtension(filename,'_asters.sav')
  save, properties,image_stack,node_table_array,ImageROI_ptrarr_ptrarr,filename=filename
  dummy = dialog_message('Finish saving file!: '+filename)
  return
endif

if keyword_set(loadsav) then begin

  filename = Dialog_Pickfile(/read,get_path=fpath,filter=['*asters.sav'],title='Select *asters.sav file to open')
  if filename eq '' then begin
    print,'filename not recognized', filename
    return
  endif
  cd,fpath
  print,'opening file: ', filename
  if strpos(filename,'_asters.sav') ne -1 then restore,filename=filename
  
  
  node_display, event,/reset
  widget_control,widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),set_value=1
  node_display,event,frame=1
  
  if (size(node_table,/dimensions))[0] lt 19 then begin
    for i =0, n_elements(node_table_array)-1 do begin
      if ptr_valid(node_table_array[i]) then begin 
        oldnode_table= *node_table_array[i]
        numrows=(size(oldnode_table,/dimensions))[1]
        oldcol = (size(oldnode_table,/dimensions))[0]
        newnodetable= dblarr(19,numrows)
        newnodetable[0:oldcol-1,*]=oldnode_table
        node_table_array[i]=ptr_new(newnodetable)
      endif      
    endfor  
  endif
  return
endif

end

pro node_loadtiff, event, filename = ff, loadflag= loadflag, noreverse = noreverse, cell=cell, dimension=dimension, image_data=image_data, single=single
  common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
  common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array
  common node_mask, ImageROI_ptrarr_ptrarr
  
  if ~keyword_set(ff) then begin
    dataFile = Dialog_Pickfile(/read,get_path=fpath,filter=['*.tif','*.tiff'],title='Select *.tif file to open')
    if dataFile eq '' then return
    cd, fpath
  endif else dataFile = ff
  
  rawimage = readtiffstack(dataFile)
  if size(rawimage,/type) eq 0 then begin
    if keyword_set(loadflag) then loadflag = -1
    return
  endif
  
  timestamp = (file_info(dataFile)).atime
  
   if ~keyword_set(noreverse) then rawImage = reverse(rawImage,2,/overwrite)
  
  dimension = size(rawimage,/dimensions)
  print, dimension
  
  properties.datafile = datafile
  properties.timestamp = timestamp
  properties.xpixels=dimension[0]
  properties.ypixels=dimension[1]
  
  if size(rawimage,/n_dimensions) eq 3 then begin
    properties.frames = dimension[2]
    image_data = rawImage[*,*,0:dimension[2]-1]
    image_stack = rawImage   
  endif else begin
    properties.frames = 1
    image_date= rawImage
    image_stack= dblarr([size(rawimage,/dimensions),1])
    image_stack[*,*,0] = rawimage    
  endelse
  print,'Load from:', datafile
  print,'timestamp:', timestamp
  print, 'frames:', properties.frames
  
  node_table_array = ptrarr(properties.frames)
  widget_control,widget_info(event.top,find_by_uname='WID_SLIDER_FRAME'),set_value=1
  node_display, event,/reset
end

function node_getzoomfactor
  common display_info, def_w, wxsz,wysz, mousedown, infoinitial, frontendwindow, frontendzoom
   common node_dataset, properties, image_stack, node_table,node_searchbuffer,node_table_array
   
    xmin = frontendzoom[0]>0
    ymin = frontendzoom[1]>0
    xmax = frontendzoom[2]<(properties.xpixels-1) 
    ymax = frontendzoom[3]<(properties.ypixels-1) 
    xdim = xmax-xmin+1 & ydim = ymax-ymin+1
    mgw = ((wxsz*1./xdim)<(wysz*1./ydim))
    return, {mgw:mgw,xmin:xmin,ymin:ymin,xrange:xdim,yrange:ydim,xmax:xmax,ymax:ymax,wxsz:wxsz,wysz:wysz,xdim:xdim,ydim:ydim}
  
end


function node_StringTokenizer, incometext,delimiter=delimiter
  if ~keyword_set(delimiter) then delimiter = " ,"
  thenumber = strsplit(incometext,delimiter,/extract,count=count)
  return, uint(thenumber)
end

pro savescreentiff, event
  filename = Dialog_Pickfile(/write,get_path=fpath)
  if strlen(fpath) ne 0 then cd,fpath
  if filename eq '' then return
  presentimage=reverse(tvrd(true=1),3)
  filename=AddExtension(filename,'.tiff')
  write_tiff,filename,presentimage,orientation=1
end


pro node_edge, image=image, mode=mode, parameters=parameters,results=results, threshold=threshold
 if ~keyword_set(image) then return 
 if ~keyword_set(mode) then mode = 0
 edgelist = ['Sobel','Roberts','Prewitt','Emboss','Edge_DOG','Laplacian','Unsharpmask']
 
 if ~keyword_set(parameters) then parameters = [3.,5.]
 if ~keyword_set(threshold) then threshold = 0.0
 radius1 = parameters[0]
 radius2 = parameters[-1]
 
 case mode of 
  0: results=sobel(image)
  1: results=roberts(image)
  2: results=prewitt(image)
  3: results=emboss(image, /edge_truncate,/nan)
  4: results=edge_dog(image, radius1=radius1, radius2=radius2)
  5: results = laplacian(image,/edge_truncate,/nan,/normalize)
  6: results= unsharp_mask(image, amount= parameters[0], radius=parameters[-1],threshold=threshold) 
 endcase
 cgimage, results, position=[0.,0.,1.,1.],/scale
end

pro node_findPeaks, image=image, windowradius=windowradius, verbose=verbose, Gauss_sigma=Gauss_sigma, maxcount=maxcount,results=results, zoomfactor=zoomfactor, mask=mask,usemask=usemask, frame=frame
  if ~keyword_set(image) then return
  if n_elements(image) lt 1 then return
  if size(image,/n_dimension) ne 2 then return
  if ~keyword_set(windowradius) then windowradius = 3
  if ~keyword_set(Gauss_sigma) then Gauss_sigma = 1.
  if ~keyword_set(maxcount) then maxcount = 10.
  if windowradius lt Gauss_sigma*3 then windowradius = ceil(gauss_sigma*3.)

  image=double(image)

  Theimage =edge_dog(image,radius1=Gauss_sigma, radius2=windowradius)
  cgimage, TheImage,position=[0.5,0.5,1,1.],/scale,/noerase
  
  dk=2*windowradius+1  & dl=windowradius
  xyvals=findgen(dk)-dl
  ;Gauss_sigma=1.3
  ;Gauss_sigma=float(d)/4
  gcx=exp(-(xyvals^2)/Gauss_sigma^2/2.)
  gausscenter=gcx#gcx
  
  z = node_getzoomfactor()
  
  wait, .1
  cgimage, theimage, position=[0.,0.,1.,1.],/scale
  tvscl, congrid(gausscenter,dk*z.mgw,dk*z.mgw)
  
  imdim = size(image,/dimensions)
  xdim = imdim[0] &   ydim = imdim[1]
  
  rad=(windowradius)
  dia=2*rad+1
  arr=indgen(dia*dia,/L64)
  radsq=rad*rad
  darr=(radsq-((arr mod dia) - rad)^2-((arr / dia) - rad)^2)>0
  ind_zero=where(darr)
  ind2d_zero=ARRAY_INDICES(intarr(dia,dia),ind_zero)
  xind_zero=ind2d_zero[0,*]-rad
  yind_zero=ind2d_zero[1,*]-rad
  
  peakxa=intarr(maxcount)
  peakya=intarr(maxcount)
  maxpeakcriteria=intarr(maxcount)
  counter = 0
  
  thesearchimage=1.*theimage-min(theimage)
  tbegin = systime(/seconds)
  print,'Search initiated: ', maxcount, ' peaks'
  while (counter lt maxcount) do begin
    maxpeakcriter=max(thesearchimage,peakloc)
    
    peakloc2d=ARRAY_INDICES(thesearchimage,peakloc)
    peakx=peakloc2d[0]
    peaky=peakloc2d[1]
    
    ; node_mask, event, index1d = peakloc, boundcheck=frame, inbound=inbound    
 
      xin_new=(xind_zero+peakx)>0<(xdim-1)
      yin_new=(yind_zero+peaky)>0<(ydim-1)
      thesearchimage[xin_new,yin_new] = 0    ; set circular vicinity to 0
      peakxa[counter] = peakx
      peakya[counter] = peaky
      maxpeakcriteria[counter] = maxpeakcriter
      counter+=1
    
      print,peakx, peaky
      scfacx = 1024./xdim
      scfacy = 1024./ydim
      cgplots, peakx*scfacx,peaky*scfacy,/device,psym = 1,color='green'

  endwhile
  st_ind=(counter-1)>0
  peakxa=temporary(peakxa[0:st_ind])
  peakya=temporary(peakya[0:st_ind])
  maxpeakcriteria=temporary(maxpeakcriteria[0:st_ind])
  
  tend = systime(/seconds)
  print,'Finish searching:', tend-tbegin, ' sec.'
  
  if ~keyword_set(zoomfactor) then begin
    mgw=1. &     xmin=0. & ymin = 0.
  endif else begin
    xmin = zoomfactor.xmin &  ymin =    zoomfactor.ymin & mgw =   zoomfactor.mgw
  endelse
  
  peakxa = xmin+peakxa
  peakya = ymin+peakya
  if keyword_set(usemask) then begin
    print,' Filtering..'
    numpeaks= n_elements(peakxa)
    peakflag = intarr(numpeaks)
    for i = 0, numpeaks-1 do begin
      node_mask, event, x = peakxa[i], y = peakya[i], boundcheck=frame, inbound=inbound ,/verbose
      peakflag[i] = inbound
      ;if inbound eq 1 then print,'in bound' else print, 'out of bound'
    endfor
    goodpeakindex = where(peakflag eq 1, count)
    print, count
    if count eq 0 then begin
      print, 'No good peaks!.'
      zz= dialog_message('No valid peak found!')
      results = {peakcount:0,x:[0],y:[0]} 
      return
    endif
    peakxa = peakxa[goodpeakindex]
    peakya = peakya[goodpeakindex]   
    results = {peakcount:n_elements(peakxa),x:peakxa,y:peakya}    
  endif else results = {peakcount:n_elements(peakxa),x:peakxa,y:peakya}
  
  cgimage, thesearchimage, /noerase,/scale,position=[0.,0.,.15,.15]
end

function getCircularMask, im, maskrad

  imdim = size(im,/dimensions)
  xdim = imdim[0]
  ydim = imdim[1]
  xcoord = findgen(xdim)
  ycoord = findgen(ydim)
  centerx = xdim*0.5
  centery = ydim*0.5
  distance = dblarr(imdim)
  for i = 0, xdim-1 do for j = 0, ydim-1 do begin
    distance[i,j] = (centerx-xcoord[i])^2+(centery-ycoord[j])^2
  endfor
  
  wheremask = where(distance ge maskrad)
  result = im*0+1.
  result[wheremask] = 0
  return, result
end



pro x_FindPeaks, clip, totdat, d, Gauss_sigma, threshold, mxcnt, peakxa, peakya, maxpeakcriteria, criteria    ;Create and ordered list of peak candidate coordinates
  if mxcnt eq 0 then return
  clipsz=size(clip)
  Nx=clipsz[1]
  Ny=clipsz[2]
  criteriaclip=(clip-0.9*smooth(totdat,3))>0.5
  ;dk=5 & dl=2
  dk=2*d+1  & dl=d
  xyvals=findgen(dk)-dl
  ;Gauss_sigma=1.3
  ;Gauss_sigma=float(d)/4
  gcx=exp(-(xyvals^2)/Gauss_sigma^2/2.)
  gausscenter=gcx#gcx
  gausscenter=gausscenter-mean(gausscenter)
  criteria=(convol(criteriaclip,gausscenter) > 0)
  newcriteria=criteria
  maxpeakcriter=max(criteria)
  
  rad=(d+3)
  dia=2*rad+1
  arr=indgen(dia*dia,/L64)
  radsq=rad*rad
  darr=(radsq-((arr mod dia) - rad)^2-((arr / dia) - rad)^2)>0
  ind_zero=where(darr)
  ind2d_zero=ARRAY_INDICES(intarr(dia,dia),ind_zero)
  xind_zero=ind2d_zero[0,*]-rad
  yind_zero=ind2d_zero[1,*]-rad
  
  peakxa=intarr(mxcnt)
  peakya=intarr(mxcnt)
  maxpeakcriteria=intarr(mxcnt)
  counter = 0
  
  ;t0=SYSTIME(/SECONDS )
  if maxpeakcriter gt threshold then begin
    while (maxpeakcriter gt threshold) and (counter lt mxcnt) do begin
      maxpeakcriter=max(newcriteria,peakloc)
      ;peakloc=where(newcriteria eq maxpeakcriter,count)
      ;if (count gt 0) and (maxpeakcriter gt threshold) then begin
      if (maxpeakcriter gt threshold) then begin
        peakloc2d=ARRAY_INDICES(newcriteria,peakloc)
        peakx=peakloc2d[0]
        peaky=peakloc2d[1]
        xin_new=(xind_zero+peakx)>0<(Nx-1)
        yin_new=(yind_zero+peaky)>0<(Ny-1)
        ;if peakx-d lt 0 then stop
        ;cir_ind=where(sqrt((Xpts-peakx)*(Xpts-peakx)+(Ypts-peaky)*(Ypts-peaky)) le (d+3))
        ;newcriteria[peakx-d:peakx+d,peaky-d:peaky+d] = 0 ; set rectangular vicinity to 0
        newcriteria[xin_new,yin_new] = 0    ; set circular vicinity to 0
        peakxa[counter] = peakx
        peakya[counter] = peaky
        maxpeakcriteria[counter] = maxpeakcriter
        counter+=1
      endif
    endwhile
    st_ind=(counter-1)>0
    peakxa=temporary(peakxa[0:st_ind])
    peakya=temporary(peakya[0:st_ind])
    maxpeakcriteria=temporary(maxpeakcriteria[0:st_ind])
  endif
  ;t1=SYSTIME(/SECONDS )
  ;print,t1-to
  mxcnt=counter
  return
end

function uppertriangle, n, nan=nan
  i = REBIN(LINDGEN(n), n, n)
  j = REBIN(TRANSPOSE(LINDGEN(n)), n, n)
  if keyword_set(nan) then begin
     upp = (i GE j)
     return, 1./upp
  endif else return, (i GE j)
end

function areaPolygon,x,y
  ;calculate area using determinants
  N=n_elements(x)
  xx = [x, x[0], x[1]]
  yy = [y, y[0], y[1]]
  area = 0.
  for i = 0, N do area=area+ xx[i]*( yy[i+1] - yy[i-1])
  return,area/2.
end

function AddExtension, filename, extension    ; checks if the filename has the extension (caracters after dot in "extension" variable. If it does not, adds the extension.
  sep = !VERSION.OS_family eq 'unix' ? '/' : '\'
  dot_pos=strpos(extension,'.',/REVERSE_OFFSET,/REVERSE_SEARCH)
  short_ext=strmid(extension,dot_pos)
  short_ext_pos=strpos(filename,short_ext,/REVERSE_OFFSET,/REVERSE_SEARCH)
  ext_pos=strpos(filename,extension,/REVERSE_OFFSET,/REVERSE_SEARCH)
  add_ext=(ext_pos lt 0)  ? extension : ''
  file_sep_pos=strpos(filename,sep,/REVERSE_OFFSET,/REVERSE_SEARCH)
  file_dot_pos=strpos(filename,'.',/REVERSE_OFFSET,/REVERSE_SEARCH)
  filename_without_ext = (file_dot_pos gt file_sep_pos) ? strmid(filename,0,file_dot_pos) :  filename
  filename_with_ext = (ext_pos gt 0) ? filename : (filename_without_ext + add_ext)
  return,filename_with_ext
end


function StripExtension, filename   ; checks if the filename has the extension (caracters after dot in "extension" variable. If it does not, adds the extension.
  sep = !VERSION.OS_family eq 'unix' ? '/' : '\'
  file_sep_pos=strpos(filename,sep,/REVERSE_OFFSET,/REVERSE_SEARCH)
  file_dot_pos=strpos(filename,'.',/REVERSE_OFFSET,/REVERSE_SEARCH)
  filename_without_ext = (file_dot_pos gt file_sep_pos) ? strmid(filename,0,file_dot_pos) :  filename
  return,filename_without_ext
end

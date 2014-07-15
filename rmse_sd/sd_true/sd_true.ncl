; ------------------------------------------
; Script     : sd_true.ncl
; Created on : 2014-7-7 16:50:26
; Author     : Wayne
; ------------------------------------------
; Description:
;   To calculate standard deviation of annual-cycle removed hgt500mb.
; ------------------------------------------
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
;=================================================================
begin
;************************************
in = addfile("myfile.nc","r")      ;pointer to file
t = in -> T       ;read in data
;***************************************
;creat plot
;*************************************
    wks = gsn_open_wks("ps","ce")   ;open ps file
    gsn_define_colormap(wks, "BlAqGrYeOrRe")   ;choose color map
    res =  True ;resource varb
    res@cnFillOn = True ; turn on color
    res@cnLinesOn = False ;no cn lines
    res@cnLevelSpacingF = 0.5 ; cn spacing
    res@gsnSpreadColors = True ;full colors
    res@lbAutoLabelStride = True ;nice lb labes
    plot = gsn_csm_contour_map_ce(wks, t, res)
end
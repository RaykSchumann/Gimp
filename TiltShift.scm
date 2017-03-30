; Tilt Shift Filter rel 0.08
; Created by Rayk Schumann @ 2015-06-22
; Comments directed to http://gimpchat.com or http://gimpscripts.com
;
; License: GPLv3
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU General Public License as published by
;    the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;    GNU General Public License for more details.
;
;    To view a copy of the GNU General Public License
;    visit: http://www.gnu.org/licenses/gpl.html
;
;
; ------------
;| Change Log |
; ------------ 
; Rel 0.07 - Initial Release
; Rel 0.08 - Bug fix standard colours

;
(define (script-fu-tilt-shift Img Layer Top Bottom Opacity Gauss)

	(if (= (car (gimp-image-is-valid Img)) TRUE) ;check if image is loaded

		(if (<(car (gimp-image-get-layer-by-name Img "Tilt shift layer1")) 0 ) ;check if layer exists

			(let* (
					(ImgWidth (car (gimp-image-width Img)))
					(ImgHeight (car (gimp-image-height Img)))
					(LayerNew (car (gimp-layer-copy Layer TRUE))) ; Layer must be set to the lowest layer	
					(LayerNew2 (car (gimp-layer-copy Layer TRUE))) ; Layer must be set to the lowest layer				
					(LayerNewMask (car (gimp-layer-create-mask LayerNew ADD-BLACK-MASK)))
					(LayerNewMask2 (car (gimp-layer-create-mask LayerNew2 ADD-BLACK-MASK)))
				)
			(gimp-image-undo-disable Img)  ;switches off undo 
			(gimp-context-push)				;saves the current context settings 
			(gimp-palette-set-background '(255 255 255)) ; sets background colour to white	
			(gimp-palette-set-foreground '(0 0 0))	; sets foreground colour to black	
			; ********** 1st gauss layer *************
			(gimp-image-insert-layer Img LayerNew 0 0)	; activates and shows new layer
			(gimp-item-set-name LayerNew "Tilt shift layer temp")
			(plug-in-gauss 1 Img LayerNew Gauss Gauss 0) ; applies the Gauss filter
			(gimp-layer-set-opacity LayerNew Opacity)
			(gimp-layer-add-mask LayerNew LayerNewMask) ; activates and shows new layer mask


			; Init gradient top
			(gimp-image-select-rectangle Img CHANNEL-OP-REPLACE 0 0 ImgWidth (/ ImgHeight 2))
			(gimp-layer-set-edit-mask LayerNew TRUE)
			(gimp-edit-blend LayerNewMask FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 1 FALSE (round(/ ImgWidth 2)) 0 (round(/ ImgWidth 2)) (round(* ImgHeight (/ Top 100))))

			; Init gradient bottom
			(gimp-image-select-rectangle Img CHANNEL-OP-REPLACE 0 (/ ImgHeight 2) ImgWidth (/ ImgHeight 2))
			(gimp-edit-blend LayerNewMask FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 1 FALSE (round(/ ImgWidth 2)) ImgHeight (round(/ ImgWidth 2)) (round(- ImgHeight (* ImgHeight (/ Bottom 100)))))
			(gimp-layer-set-edit-mask LayerNew FALSE) 

			(gimp-selection-none Img)	
			(gimp-item-set-name LayerNew "Tilt shift layer1")

			
			; ************ 2nd gauss layer (double Gauss filter) ************
			(gimp-image-insert-layer Img LayerNew2 0 0)	; activates and shows new layer
			(gimp-item-set-name LayerNew2 "Tilt shift layer temp")
			(plug-in-gauss 1 Img LayerNew2 (* Gauss 2) (* Gauss 2) 0) ; applies the Gauss filter
			(gimp-layer-set-opacity LayerNew2 Opacity)
			(gimp-layer-add-mask LayerNew2 LayerNewMask2) ; activates and shows new layer mask

			; Init gradient top
			(gimp-image-select-rectangle Img CHANNEL-OP-REPLACE 0 0 ImgWidth (/ ImgHeight 2))
			(gimp-layer-set-edit-mask LayerNew2 TRUE)
			(gimp-edit-blend LayerNewMask2 FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 1 FALSE (round(/ ImgWidth 2)) 0 (round(/ ImgWidth 2)) (round(* ImgHeight (/ (/ Top 2) 100))))

			; Init gradient bottom
			(gimp-image-select-rectangle Img CHANNEL-OP-REPLACE 0 (/ ImgHeight 2) ImgWidth (/ ImgHeight 2))
			(gimp-edit-blend LayerNewMask2 FG-BG-RGB-MODE NORMAL-MODE GRADIENT-LINEAR 100 0 REPEAT-NONE TRUE FALSE 1 1 FALSE (round(/ ImgWidth 2)) ImgHeight (round(/ ImgWidth 2)) (round(- ImgHeight (* ImgHeight (/ (/ Bottom 2) 100)))))
			(gimp-layer-set-edit-mask LayerNew2 FALSE) 

			(gimp-selection-none Img)	
			(gimp-item-set-name LayerNew2 "Tilt shift layer2")

			(gimp-context-pop)
			(gimp-image-undo-enable Img)
			(gimp-displays-flush)	
			
			)
			(gimp-message "Please delete layer 'Tilt shift layer1/2' first.")

		)		
		
		(gimp-message "Please load image first.")
	)	
)
; Im GIMP und im Menü registrieren
(script-fu-register 
    "script-fu-tilt-shift" 		;func name
    "<Toolbox>/Xtns/Tilt Shift"      ;menu label
    "Description"              			;description
    "Rayk Schumann"                       ;author
    "Copyright(c)2015, Rayk Schumann"     ;copyright notice
    "June 22nd 2015"                ;date created
    ""                     				;image type that the script works on

    SF-IMAGE      "Image" 0
    SF-DRAWABLE   "Layer" 0
	SF-ADJUSTMENT "Top    % border" '(30 10 100 1 10 0 0) 
	SF-ADJUSTMENT "Bottom % border" '(30 10 100 1 10 0 0) 
	SF-ADJUSTMENT "Opacity" '(100 0 100 1 10 0 0)
	SF-ADJUSTMENT "Gauss factor" '(40 1 100 1 10 0 0)
	)

(define (equation2 x)
  (if (< x 1) 
      0
      (if (> x 2)
          (+ (- (* x x) x) 4)
          (/ 1 x)))) 
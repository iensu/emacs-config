(add-hook 'groovy-mode-hook
	  '(lambda ()
	     (require 'groovy-electric)
	     (groovy-electric-mode)))

;;; Swedish calendar settings
;;; Taken from sv-kalender.el: http://bigwalter.net/daniel/elisp/sv-kalender.el

;; Göm vissa helgdagar?
(defvar sv-hide-some-holidays nil
  "Non-nil means some holidays won't show in the calendar.
Om icke-nil, göm vissa helgdagar i kalendern.")

;; Påskdagen (from holiday-easter-etc)
(defun sv-easter (year)
  "Calculate the date for Easter in YEAR.
Beräkna påskdagen för år YEAR."
  (let* ((century (1+ (/ year 100)))
         (shifted-epact (% (+ 14 (* 11 (% year 19))
                              (- (/ (* 3 century) 4))
                              (/ (+ 5 (* 8 century)) 25)
                              (* 30 century))
                           30))
         (adjusted-epact (if (or (= shifted-epact 0)
                                 (and (= shifted-epact 1)
                                      (< 10 (% year 19))))
                             (1+ shifted-epact)
                           shifted-epact))
         (paschal-moon (- (calendar-absolute-from-gregorian
                           (list 4 19 year))
                          adjusted-epact)))
    (calendar-dayname-on-or-before 0 (+ paschal-moon 7))))

(setq holiday-general-holidays
      '((holiday-fixed 1 1 "Nyårsdagen")
        (holiday-fixed 1 6 "Trettondedag jul")

        ;; Påsk och pingst
        (holiday-filter-visible-calendar
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (sv-easter displayed-year) (car dag)))
                  (cadr dag)))
          '((  -2 "Långfredagen")
            (  -1 "Påskafton")
            (   0 "Påskdagen")
            (  +1 "Annandag påsk")
            ( +39 "Kristi himmelfärdsdag")
            ( +49 "Pingstdagen")
            ( +50 "Annandag pingst"))))

        (holiday-fixed 5 1 "Första maj")

        (let ((midsommar-d (calendar-dayname-on-or-before
                            6 (calendar-absolute-from-gregorian
                               (list 6 26 displayed-year)))))
          ;; Midsommar
          (holiday-filter-visible-calendar
           (list
            (list
             (calendar-gregorian-from-absolute (1- midsommar-d))
             "Midsommarafton")
            (list
             (calendar-gregorian-from-absolute midsommar-d)
             "Midsommardagen")
            ;; Alla helgons dag
            (list
             (calendar-gregorian-from-absolute
              (calendar-dayname-on-or-before
               6 (calendar-absolute-from-gregorian
                  (list 11 6 displayed-year))))
             "Alla helgons dag"))))

        (holiday-fixed 12 25 "Juldagen")
        (holiday-fixed 12 26 "Annandag jul")))

;; Andra högtider
(setq holiday-other-holidays
      '((holiday-fixed 1 13 "Tjugondag Knut")
        (unless sv-hide-some-holidays
          (holiday-fixed 1 28 "Konungens namnsdag"))
        (unless sv-hide-some-holidays
          (holiday-fixed 2 2 "Kyndelsmässodagen"))
        (holiday-fixed 2 14 "Alla hjärtans dag")

        ;; Fettisdagen
        (holiday-filter-visible-calendar
         (list
          (list
           (calendar-gregorian-from-absolute
            (calendar-dayname-on-or-before
             2 (- (sv-easter displayed-year) 47)))
           "Fettisdagen")))

        (holiday-fixed 3 8 "Internationella kvinnodagen")
        (unless sv-hide-some-holidays
          (holiday-fixed 3 12 "Kronprinsessans namnsdag"))
        (holiday-fixed 3 25 "Vårfrudagen")

        ;; Mer påsk
        (holiday-filter-visible-calendar
         (mapcar
          (lambda (dag)
            (list (calendar-gregorian-from-absolute
                   (+ (sv-easter displayed-year) (car dag)))
                  (cadr dag)))
          (if sv-hide-some-holidays
              '(( -3 "Skärtorsdagen"))
            '(( -7 "Palmsöndagen")
              ( -4 "Dymmelonsdagen")
              ( -3 "Skärtorsdagen")))))

        (unless sv-hide-some-holidays
          (holiday-fixed 4 30 "Konungens födelsedag"))
        (unless sv-hide-some-holidays
          (holiday-fixed 4 1 "Första april"))
        (holiday-fixed 4 30 "Valborgsmässoafton")
        (holiday-float 5 0 -1 "Mors dag")
        (holiday-fixed 6 6 "Sveriges nationaldag")
        (unless sv-hide-some-holidays
          (holiday-fixed 7 14 "Kronprinsessans födelsedag"))
        (unless sv-hide-some-holidays
          (holiday-fixed 8 8 "Drottningens namnsdag"))
        (holiday-fixed 10 24 "FN-dagen")
        (holiday-float 11 0 2 "Fars dag")
        (unless sv-hide-some-holidays
          (holiday-fixed 11 6 "Gustaf Adolfsdagen"))
        (holiday-fixed 11 10 "Mårtensafton")
        (holiday-float 12 0 -4 "Första advent" 24)
        (holiday-float 12 0 -3 "Andra advent" 24)
        (holiday-float 12 0 -2 "Tredje advent" 24)
        (holiday-float 12 0 -1 "Fjärde advent" 24)
        (holiday-fixed 12 10 "Nobeldagen")
        (holiday-fixed 12 13 "Lucia")
        (unless sv-hide-some-holidays
          (holiday-fixed 12 23 "Drottningens födelsedag"))
        (holiday-fixed 12 24 "Julafton")
        (holiday-fixed 12 31 "Nyårsafton")))

;; Solstånd, dagjämningar, vinter- och sommartid
(setq holiday-solar-holidays
      (if sv-hide-some-holidays
          nil
        '((if (fboundp 'atan)
              (solar-equinoxes-solstices))
          (if (progn
                (require 'cal-dst)
                t)
              (funcall 'holiday-sexp calendar-daylight-savings-starts
                       '(format "Sommartid börjar %s"
                                (if
                                    (fboundp 'atan)
                                    (solar-time-string
                                     (/ calendar-daylight-savings-starts-time
                                        (float 60))
                                     calendar-standard-time-zone-name)
                                  ""))))
          (funcall 'holiday-sexp calendar-daylight-savings-ends
                   '(format "Vintertid börjar %s"
                            (if
                                (fboundp 'atan)
                                (solar-time-string
                                 (/ calendar-daylight-savings-ends-time
                                    (float 60))
                                 calendar-daylight-time-zone-name)
                              ""))))))

;; Listan med kalenderns helgdagar
(setq calendar-holidays
      (append holiday-general-holidays holiday-local-holidays
              holiday-other-holidays holiday-solar-holidays))

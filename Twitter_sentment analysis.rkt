#lang racket
(require net/url)
(require data-science-master)
(require plot)
(require math)

;Reading the Country dataset
(define Country_twitter_data (read-csv "1year_uganda.csv" #:->number? #f #:header? #t))

;Defining initial procedures
(define test_data
  (let ([tmp
         (map (λ (var1) (list (list-ref var1 5)))
              Country_twitter_data)])
    (filter (λ (z)
              (not (string-prefix? (first z) "RT"))) tmp)))

;(defining test_data_flt)
(define test_data_flt (flatten test_data))

;(take test_data 5)
(define tweets_all (apply string-append test_data_flt))

 ;Cleaning the data)
(define tweets
  (string-normalize-spaces
   (remove-punctuation
    (string-downcase tweets_all) #:websafe? #t)))

;Checking for sorted indexes ordered by coloumn
(define tweets_text (document->tokens tweets #:sort? #t))

;(sentimental selection processing)
(define sentiment (list->sentiment tweets_text #:lexicon 'nrc)) 
(aggregate sum ($ sentiment 'sentiment) ($ sentiment 'freq))

;Gettings totals and frequencies
(let ([counts
       (aggregate sum ($ sentiment 'sentiment)($ sentiment 'freq))])
  (parameterize ((plot-width 800))
;plotting settings
    (plot (list
	   (tick-grid)
	   (discrete-histogram
	    (sort counts (λ (var1 var2) (> (second var1) (second var2))))
	    #:color "Darkgreen"
	    #:line-color "Darkgreen"))
	    #:x-label "Moods (Affective Label)"
	    #:y-label "Frequency")))

(define bng-sentiment (list->sentiment tweets_text #:lexicon 'bing))
(parameterize ([plot-height 200])
  (plot (discrete-histogram
	 (aggregate sum ($ bng-sentiment 'sentiment) ($ bng-sentiment 'freq))
	 #:y-min 0
	 #:y-max 8000
	 #:invert? #t
	 #:color "Darkblue"
	 #:line-color "Darkblue")
	 #:x-label "Frequency"
	 #:y-label "Sentiment Polarity"))
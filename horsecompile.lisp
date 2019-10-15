
  (use-package :sqlite)
  (use-package :iterate)

(defun main (argv)
(defvar *db* (connect ":memory:"))

(execute-non-query *db* "create table plays (type text, foldername text, fullname text, pictures text)")


(defun sql-helper () 
  
     
       (defparameter *PLAY_TYPE* (string-downcase (execute-single *db* "select type from plays where pictures = ?" (string (caddr *parsed_dir*)))))
       (defparameter *PLAY_NAME_DIR* (string-downcase (execute-single *db* "select foldername from plays where pictures = ?" (string (caddr *parsed_dir*)))))
       (defparameter *PICTURE* (string-downcase (execute-single *db* "select pictures from plays where pictures = ?" (string (caddr *parsed_dir*)))))

)

(defun directory-to-sql (dir-with-pictures)
	   (loop for x in dir-with-pictures
	      do     
	(defparameter *directory_full* (namestring x))
		(defparameter *parsed_dir*  (read-from-string (concatenate 'string "(" (substitute #\SPACE #\/ (subseq *directory_full* 36)) ")")))
		(defparameter *PLAY_NAME* (string (with-open-file (in (string (concatenate 'string "\/home\/nyaa\/Desktop\/horses\/horselife\/" (string-downcase (car *parsed_dir*)) "\/" (string-downcase (cadr *parsed_dir*)) "\/fullname.txt")))
						     (read-line in))))
		(execute-non-query *db* "insert into plays (type, foldername, pictures, fullname) values (?,?,?,?)"
				   (string (car *parsed_dir*)) (string (cadr *parsed_dir*)) (string (caddr *parsed_dir*)) *PLAY_NAME*)
		
	     
	     

		(sql-helper)
		
		(print (execute-single *db* "select fullname from plays where pictures = ?" (string (caddr *parsed_dir*))))

		(print (concatenate 'string  *PICTURE* " " *PLAY_NAME_DIR* " " *PLAY_TYPE* " " *PLAY_NAME*))
))




(defun sql-to-html ()

  (defparameter *PLAYS_LIST* (remove-duplicates (iter (for (foldername) in-sqlite-query "select foldername from plays" on-database *db*)
      (collect (list foldername))) :test #'equal))
(print *PLAYS_LIST*)
  (loop for x in *PLAYS_LIST*
     do
       (defparameter *PLAY_NAME* (execute-single *db* "select fullname from plays where foldername = ?" (string (car x))))

       (defparameter *PLAY_TYPE* (string-downcase (execute-single *db* "select type from plays where foldername = ?" (string (car x)))))
       (defparameter *PLAY_NAME_DIR* (string-downcase (car x)))
       (defparameter *PICTURE_LIST* (remove '("FULLNAME.TXT") (iter (for (pictures) in-sqlite-query "select pictures from plays where fullname = ?" on-database *db* with-parameters (*PLAY_NAME*))
      (collect (list pictures))) :test #'equal))
(print *PICTURE_LIST*)
       (with-open-file (playhtml (string (concatenate 'string "\/home\/nyaa\/Desktop\/horses\/horselife\/" (string-downcase (car x)) ".html")) :direction :output :if-exists :supersede :if-does-not-exist :create)
	 (with-open-file (template (string (concatenate 'string "\/home\/nyaa\/Desktop\/horses\/horselife\/gallery_front_end_template.lhtml")))
	   (loop for line = (read-line template nil nil)
	      until (string= line "<b style=\"font-family:calibri\">PLAY_NAME<\/b><br>")
	      do
		
		(write-line line playhtml)

		)
	   (write-line (concatenate 'string "<b style=\"font-family:calibri\">" *PLAY_NAME* "<\/b><br>") playhtml)
	   (loop for line = (read-line template nil nil)
	      until (string= line "<PICTURE_CODE>")
	      do
		
		(write-line line playhtml)

		)
	   (loop for x in *PICTURE_LIST*
		do (write-line (concatenate 'string "<a href=\"" *PLAY_TYPE* "\/" *PLAY_NAME_DIR* "\/" (string-downcase (car x)) "\"><img src=\"" *PLAY_TYPE* "\/" *PLAY_NAME_DIR* "\/" (string-downcase (car x)) "\" alt=\"" *PLAY_NAME* "\" style=\"height:400px;\"><\/a>" ) playhtml)
		
)
	   (loop for line = (read-line template nil nil)
	      until (string= line "<\/html>")
	      do
		
		(write-line line playhtml)

		)
	   (write-line "<\/html>" playhtml)


	   ))))


(defun update-pages ()

  (defparameter *TYPE_LIST* (remove-duplicates (iter (for (foldername) in-sqlite-query "select type from plays" on-database *db*)
      (collect (list foldername))) :test #'equal))
(print *TYPE_LIST*)
  (loop for x in *TYPE_LIST*
     do
       (defparameter *PLAY_LIST_BY_TYPE* (remove-duplicates (iter (for (foldername) in-sqlite-query "select foldername from plays where type = ?" on-database *db* with-parameters ((string (car x))))
							   (collect (list foldername))) :test #'equal))
      (print *PLAY_LIST_BY_TYPE*)
       (with-open-file (playhtml (string (concatenate 'string "\/home\/nyaa\/Desktop\/horses\/horselife\/" (string-downcase (car x)) ".html")) :direction :output :if-exists :supersede :if-does-not-exist :create)
	 (with-open-file (template (string (concatenate 'string "\/home\/nyaa\/Desktop\/horses\/horselife\/" (string-downcase (car x)) "_template.lhtml")))
	   (loop for line = (read-line template nil nil)
	      until (string= line "<b style=\"font-family:calibri\">TYPE Productions<\/b><br>")
	      do
		
		(write-line line playhtml)
		(print "meow")
		)
	   (write-line (concatenate 'string "<b style=\"font-family:calibri\">" (string-capitalize (car x)) " Productions<\/b><br>") playhtml)
	   (loop for line = (read-line template nil nil)
	      until (string= line "<PICTURE_CODE>")
	      do
		
		(write-line line playhtml)
		(print "nyaa")
		)
	   (loop for y in *PLAY_LIST_BY_TYPE*
	      do 
		(print "nyaaaaaaaaa")
		(defparameter *PLAY_NAME* (execute-single *db* "select fullname from plays where foldername = ?" (string (car y))))
		(defparameter *PLAY_TYPE* (string-downcase (execute-single *db* "select type from plays where foldername = ?" (string (car y)))))
		(defparameter *PICTURE* (string-downcase (caar (remove '("FULLNAME.TXT") (iter (for (pictures) in-sqlite-query "select pictures from plays where fullname = ?" on-database *db* with-parameters (*PLAY_NAME*))
											       (collect (list pictures))) :test #'equal))))
		(defparameter *PLAY_NAME_DIR* (string-downcase (car y)))

		(write-line (concatenate 'string "<a href=\"" *PLAY_NAME_DIR* ".html\">") playhtml)
		(write-line "<figure style=\"display:inline-block;\">" playhtml)
		(write-line (concatenate 'string "<img src=\"" *PLAY_TYPE* "\/" *PLAY_NAME_DIR* "\/" *PICTURE* "\" alt=\"" *PLAY_NAME* "\" style=\"height:400px;\">") playhtml)
		(write-line (concatenate 'string "<figcaption>" *PLAY_NAME* "<\/figcaption>") playhtml)
		(write-line "<\/figure>" playhtml)
		(write-line "<\/a>" playhtml)
		
)
	   (loop for line = (read-line template nil nil)
	      until (string= line "<\/html>")
	      do
		
		(write-line line playhtml)

		)
	   (write-line "<\/html>" playhtml)


	   ))))







  
  (directory-to-sql (directory "/home/nyaa/Desktop/horses/horselife/*/*/*.*"))
  (sql-to-html)
  (update-pages)


  (execute-non-query *db* "drop table plays")
  (disconnect *db*)
)

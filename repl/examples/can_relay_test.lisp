
(defun can-handler ()
  (recv
    ((event-can-eid (? id) (? data))
     { (print "CAN eid: " id " data: " data "\n")
       (can-handler) })))

(event-register-handler (spawn can-handler))

(if (can-upload-relay)
    (print "Relay script uploaded to bridge. Listening for CAN frames...\n")
  (print "Upload failed\n"))

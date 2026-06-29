; can_relay_test.lisp
;
; Uploads the CAN relay script to the connected bridge VESC, then
; prints every CAN extended-frame event received from the bus.
;
; The relay script runs on the bridge and forwards directed CAN frames
; back over USB as COMM_LISP_RMSG packets, which the repl turns into
; event-can-eid events for any registered event handler.
;
; Usage:
;   ./repl --can=/dev/ttyACM0 -s examples/can_relay_test.lisp

(defun can-handler ()
  (recv
    ((event-can-eid (? id) (? data))
     { (print "CAN eid: " id " data: " data "\n")
       (can-handler) })))

(event-register-handler (spawn can-handler))

(if (can-upload-relay)
    (print "Relay script uploaded to bridge. Listening for CAN frames...\n")
  (print "Upload failed - is a bridge connected? (use --can=PORT)\n"))

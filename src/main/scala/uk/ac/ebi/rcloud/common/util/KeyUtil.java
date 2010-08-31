package uk.ac.ebi.rcloud.common.util;

import javax.swing.*;
import java.awt.event.KeyEvent;
import java.util.HashMap;

/**
 * 
 * User: andrew
 * Date: Oct 26, 2009
 */
public class KeyUtil {
    //private static Logger log = LoggerFactory.getLogger(KeyUtil.class);

    private static HashMap<KeyStroke, String> map = new HashMap<KeyStroke, String>();
    private static boolean onMac = System.getProperty("os.name").toLowerCase().contains("mac"); //OsUtil.isMacOs();
    public static boolean debug = false;


    public static KeyStroke getKeyStroke(int i0, int i1) {

        if ((i1 & KeyEvent.META_MASK) != 0) {
            if (!onMac) {
                i1 = i1 - KeyEvent.META_MASK + KeyEvent.CTRL_MASK;
            }
        }

        KeyStroke ks = KeyStroke.getKeyStroke(i0, i1);

        if (debug) {
            Exception ex = new Exception();
            StackTraceElement[] trace = ex.getStackTrace();
            String caller = trace[1].toString();
            if (map.containsKey(ks)) {
                //log.info("ks (" + ks.toString() + ") being mapped in (" + caller + ") already mapped in (" + map.get(ks) + ")");
            } else {
                map.put(ks, caller);
            }
        }
        return ks;
    }
}
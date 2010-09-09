package uk.ac.ebi.rcloud.workbench.extraconsole;

import uk.ac.ebi.rcloud.common.util.KeyUtil;

import javax.swing.*;
import javax.swing.event.DocumentListener;
import javax.swing.event.EventListenerList;
import javax.swing.event.DocumentEvent;
import javax.swing.text.*;
import java.awt.event.*;
import java.awt.*;
import java.util.HashMap;

/**
 * User: andrew
 * Date: Sep 28, 2009
 */
public class ConsolePanelBase extends JTextPane {
    //final private Logger log = LoggerFactory.getLogger(getClass());

    public static final String INPUTSTART = "INPUTSTART";

    public String consolePrompt = "#";

    //public static final Object Input = new Object();
    //public static final Object Actions = new Object();
    //private static final Cursor MoveCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
    //private static final Cursor DefaultCursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);

    private EventListenerList listenerList;
    private DocumentHandler documentHandler;
    private ConsoleHistoryText history;

    public ConsolePanelBase() {

        history = new ConsoleHistoryText(this);

        listenerList = new EventListenerList();

        initActions();

        addMouseListener(new ConsoleMouseAdapter());

        documentHandler = new DocumentHandler();

        setDocument(getDocument());
        
        setInputStart(0);
    }

    public void setDocument(Document doc){
        if(documentHandler != null && getDocument() != null)
            getDocument().removeDocumentListener(documentHandler);

        super.setDocument(doc);
        doc.addDocumentListener(documentHandler);
    }

    public void addActionListener(ActionListener l) {
        listenerList.add(ActionListener.class,l);
    }

    public void removeActionListener(ActionListener l) {
        listenerList.remove(ActionListener.class,l);
    }

    public void fireActionEvent(String code) {
        lockConsole();

        ActionEvent evt = new ActionEvent(this,
            ActionEvent.ACTION_PERFORMED,code);

        Object[] listeners = listenerList.getListenerList();
        for(int i = 0; i < listeners.length; i++) {
            if(listeners[i] == ActionListener.class) {
                ActionListener l = (ActionListener)
                    listeners[i+1];
                l.actionPerformed(evt);
            }
        }
    }

    public String getInput() {
        try {
            Document doc = getDocument();
            int cmdStart = getInputStart();
            String line = doc.getText(cmdStart,doc.getLength() - cmdStart);
            if(line.endsWith("\n"))
                return line.substring(0,line.length() - 1);
            else
                return line;
        } catch(BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    public void setInput(String line) {
        try {
            Document doc = getDocument();
            int cmdStart = getInputStart();
            doc.remove(cmdStart,doc.getLength() - cmdStart);
            doc.insertString(cmdStart,line,null);
        } catch(BadLocationException e) {
            throw new RuntimeException(e);
        }
    }

    public int getInputStart() {
        return (Integer) getDocument().getProperty(INPUTSTART);
    }

    public void setInputStart(int cmdStart) {
        getDocument().putProperty(INPUTSTART, cmdStart);
    }

    public String getInputBeforeCaret() {
        try {
            return getDocument().getText(getInputStart(),
                getCaretPosition() - getInputStart());
        } catch(BadLocationException e) {
            throw new RuntimeException(e);
        }
    }
    
    public void printPrompt() {

        //log.error("printPrompt");

        //writeAttrs(ConsolePanelBase.colorAttributes(Color.RED.darker().darker()), consolePrompt);
        writeAttrs(ConsolePanelBase.colorAttributes(Color.BLACK), consolePrompt);
        //writeAttrs(ConsolePanelBase.colorAttributes(Color.BLACK), " ");

        unlockConsole();
    }

    public void printPrompt(String prompt) {
        setPrompt(prompt);
        printPrompt();
    }

    public void print(String msg) {
        if (msg != null && !msg.isEmpty()) writeAttrs(ConsolePanelBase.colorAttributes(Color.BLACK), msg);
    }

    public void print(Color color, String msg) {
        writeAttrs(ConsolePanelBase.colorAttributes(color),
            msg);
    }

    public void lockConsole() {
        if(SwingUtilities.isEventDispatchThread()) {
            setEditable(false);
        } else {
            EventQueue.invokeLater(new Runnable() {
                public void run() {
                    setEditable(false);
                }
            });
        }
    }

    public void unlockConsole() {
        if(SwingUtilities.isEventDispatchThread()) {
            setEditable(true);
            setCaretPosition(getDocument().getLength());
        } else {
            EventQueue.invokeLater(new Runnable() {
                public void run() {
                    setEditable(true);
                    setCaretPosition(getDocument().getLength());
                }
            });
        }
    }

    public boolean isConsoleLocked() {
        return !isEditable();
    }

    public void writeAttrs(final AttributeSet attrs, final String msg) {
        if(SwingUtilities.isEventDispatchThread())
            writeSafely(attrs,msg);
        else {
            EventQueue.invokeLater(new Runnable() {
                public void run() {
                    writeSafely(attrs,msg);
                }
            });
        }
    }

    public void writeAttrsBeforeInput(final AttributeSet attrs, final String msg) {
        if(SwingUtilities.isEventDispatchThread())
            writeSafelyBeforeInput(attrs,msg);
        else {
            EventQueue.invokeLater(new Runnable() {
                public void run() {
                    writeSafelyBeforeInput(attrs,msg);
                }
            });
        }
    }

    private void writeSafely(AttributeSet attrs, String msg) {

        //log.error("writeSafely");

        Document scrollback = getDocument();
        try {
            if(attrs != null && StyleConstants.getIcon(attrs) != null)
                msg = " ";
            scrollback.insertString(scrollback.getLength(),
                msg,attrs);
        } catch(BadLocationException bl) {
            throw new RuntimeException(bl);
        }

        setInputStart(scrollback.getLength());
    }

    private void writeSafelyBeforeInput(AttributeSet attrs, String msg) {
        Document scrollback = getDocument();
        try {
            if(attrs != null && StyleConstants.getIcon(attrs) != null)
                msg = " ";
            scrollback.insertString(getInputStart() - getPrompt().length(),
                msg,attrs);
        } catch(BadLocationException bl) {
            throw new RuntimeException(bl);
        }
    }

    public void setPrompt(String prompt) {
        consolePrompt = prompt;
    }

    public String getPrompt() {
        return consolePrompt;
    }

    public static AttributeSet colorAttributes(Color color) {
        SimpleAttributeSet style = new SimpleAttributeSet();

        if(color != null)
            style.addAttribute(StyleConstants.Foreground,color);

        return style;
    }

    class DocumentHandler implements DocumentListener {
        public void insertUpdate(DocumentEvent e) {

            int offset = e.getOffset();
            int length = e.getLength();

            int cmdStart = getInputStart();
            if(offset < cmdStart)
                cmdStart += length;
            setInputStart(cmdStart);

        }

        public void removeUpdate(DocumentEvent e) {
            int offset = e.getOffset();
            int length = e.getLength();

            int cmdStart = getInputStart();
            if(offset < cmdStart) {
                if(offset + length > cmdStart)
                    cmdStart = offset;
                else
                    cmdStart -= length;
            }
            setInputStart(cmdStart);
        }

        public void changedUpdate(DocumentEvent e) {}
    }

    //   P O P U P   M E N U   S U P P O R T
    //

    class ConsoleMouseAdapter extends MouseAdapter {
        public void mousePressed(MouseEvent e) {
            checkPopup(e);
        }

        public void mouseClicked(MouseEvent e) {
            checkPopup(e);
        }

        public void mouseReleased(MouseEvent e) {
            checkPopup(e);
        }

        private void checkPopup(MouseEvent e) {
            if (e.isPopupTrigger()) {
                JPopupMenu popupMenu = initConsolePopupMenu();
                popupMenu.show(ConsolePanelBase.this, e.getX(), e.getY());
            }
        }
    }


    public boolean addMenuItem(JComponent menu, String actionname) {

        JMenuItem item = menuItemsMap.get(actionname);

        if (item == null) {
            throw new RuntimeException("Menu item not defined " + actionname);
        }

        if (item.getAction().isEnabled()) {
            item.setEnabled(true);
            menu.add(item);
            return true;
        }

        return false;
    }

    public JPopupMenu initConsolePopupMenu() {

        JPopupMenu menu = new JPopupMenu();

        addMenuItem(menu, COPY);
        addMenuItem(menu, CUT);
        addMenuItem(menu, PASTE);
        addMenuItem(menu, CLEAR);
        menu.addSeparator();

        addMenuItem(menu, NEXT);
        addMenuItem(menu, PREV);
        addMenuItem(menu, SEARCH);

        return menu;
    }

    //   A C T I O N   R O U T I N E S
    //

    private static String COPY = "console-copy";
    private static String CUT = "console-cut";
    private static String PASTE = "console-paste";
    private static String CLEAR = "console-clear";
    private static String ENTER = "console-enter";
    private static String BACKSPACE = "console-backspace";
    private static String DELETE = "console-delete";
    private static String BEGINLINE = "console-home";
    private static String BEGINLINESELECT = "console-home-select";
    private static String NEXT = "next-command";
    private static String PREV = "prev-command";
    private static String SEARCHNEXT = "search-next-command";
    private static String SEARCHPREV = "search-prev-command";
    private static String SEARCH = "search-command";
    private static String BACKWARD = "console-backward";
    private static String BACKWARDSELECT = "console-backward-select";
    private static String EXPORT2PDF = "export-to-pdf";

    public void initActions() {

        initAction(COPY, new CopyAction("Copy",
                KeyUtil.getKeyStroke(KeyEvent.VK_C, KeyEvent.META_MASK)),
                "/views/images/extraconsole/edit-copy.png");

        initAction(CUT, new CutAction("Cut",
                KeyUtil.getKeyStroke(KeyEvent.VK_X, KeyEvent.META_MASK)),
                "/views/images/extraconsole/edit-cut.png");

        initAction(PASTE, new PasteAction("Paste",
                KeyUtil.getKeyStroke(KeyEvent.VK_V, KeyEvent.META_MASK)),
                "/views/images/extraconsole/edit-paste.png");

        initAction(CLEAR, new ClearScreenAction("Clear Screen",
                null),
                "/views/images/extraconsole/edit-clear.png");

        initAction(ENTER, new EnterAction("Enter",
                KeyUtil.getKeyStroke(KeyEvent.VK_ENTER,0)),
                null);

        initAction(BACKSPACE, new BackspaceAction("Backspace",
                KeyUtil.getKeyStroke(KeyEvent.VK_BACK_SPACE,0)),
                null);

        initAction(DELETE, new DeleteAction("Delete",
                KeyUtil.getKeyStroke(KeyEvent.VK_DELETE,0)),
                null);

        initAction(BEGINLINE, new BeginLineAction("Begin Line",
                KeyUtil.getKeyStroke(KeyEvent.VK_HOME, 0), false),
                null);

        initAction(BEGINLINESELECT, new BeginLineAction("Begin Line & Select",
                KeyUtil.getKeyStroke(KeyEvent.VK_HOME, InputEvent.SHIFT_MASK), true),
                null);

        initAction(NEXT, new NextCommandAction("Next Command",
                KeyUtil.getKeyStroke(KeyEvent.VK_DOWN, 0)),
                null);

        initAction(PREV, new PrevCommandAction("Prev Command",
                KeyUtil.getKeyStroke(KeyEvent.VK_UP, 0)),
                null);

        initAction(SEARCHNEXT, new SearchNextAction("Search Next",
                KeyUtil.getKeyStroke(KeyEvent.VK_DOWN, InputEvent.META_MASK)),
                null);

        initAction(SEARCHPREV, new SearchPrevAction("Search Prev",
                KeyUtil.getKeyStroke(KeyEvent.VK_UP, InputEvent.META_MASK)),
                null);

        initAction(SEARCH, new SearchPrevAction("Search Command",
                KeyUtil.getKeyStroke(KeyEvent.VK_R, InputEvent.CTRL_MASK)),
                null);

        initAction(BACKWARD, new CaretBackwardAction("Backward",
                KeyUtil.getKeyStroke(KeyEvent.VK_LEFT, 0)),
                null);

        initAction(BACKWARDSELECT, new CaretBackwardSelectAction("Backward Select",
                KeyUtil.getKeyStroke(KeyEvent.VK_LEFT, InputEvent.SHIFT_MASK)),
                null);

        String CONSOLE_KEYMAP = "extra-console-keymap";
        Keymap binding = addKeymap(CONSOLE_KEYMAP, getKeymap());
        binding.setDefaultAction(new NewDefaultKeyTypedAction("Default", null));
        setKeymap(binding);

    }

    private void createMenuItem(String actionname, String iconPath) {

        ConsoleAction action = consoleActions.get(actionname);

        JMenuItem menuItem = new JMenuItem();

        if (action != null) {
            menuItem.setAction(action);

            if (action.getKeystroke() != null) {
                menuItem.setAccelerator(action.getKeystroke());
            }
        }
//        if (iconPath != null) {
//            menuItem.setIcon(new ImageIcon(ImageLoader.load(iconPath)));
//        }

        menuItemsMap.put(actionname, menuItem);
    }

    public void registerKeyAction(String actionname) {
        ConsoleAction action = consoleActions.get(actionname);
        KeyStroke ks = action.getKeystroke();

        if (ks != null) {
            getInputMap().put(ks, actionname);
            getActionMap().put(actionname, action);
        }
    }

    public void initAction(String name, ConsoleAction action, String iconPath) {

        consoleActions.put(name, action);

        createMenuItem(name, iconPath);

        registerKeyAction(name);
    }


    //   A C T I O N S
    //
    //

    public abstract class ConsoleAction extends AbstractAction {
        private KeyStroke keystroke = null;

        public ConsoleAction(String name, KeyStroke keystroke){
            super(name);
            this.keystroke = keystroke;
        }

        public KeyStroke getKeystroke() {
            return keystroke;
        }

        public void setKeystroke(KeyStroke keystroke) {
            this.keystroke = keystroke;
        }
    }

    class ClearScreenAction extends ConsoleAction {
        public ClearScreenAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent event) {
            Document doc = getDocument();

            try {
                getDocument().remove(0, doc.getLength());
            } catch (BadLocationException ble) {
            }

            printPrompt();
        }

        public boolean isEnabled() {
            return getDocument().getLength() > 0;
        }
    }

    class BackspaceAction extends ConsoleAction {
        private Action delegate = getActionMap().get(DefaultEditorKit.deletePrevCharAction);

        public BackspaceAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            int inputStart = getInputStart();

            if(getCaretPosition() > inputStart &&
               getSelectionStart() > inputStart) {
                delegate.actionPerformed(evt);
            }
        }
    }

    class DeleteAction extends ConsoleAction {
        private Action delegate = getActionMap().get(DefaultEditorKit.deleteNextCharAction);

        public DeleteAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            int inputStart = getInputStart();

            if(getCaretPosition() >= inputStart &&
               getSelectionStart() >= inputStart) {
                delegate.actionPerformed(evt);
            }
        }
    }

    class EnterAction extends ConsoleAction {
        public EnterAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            setCaretPosition(getDocument().getLength());
            replaceSelection("\n");

            String input = getInput();

            if (input.length() > 0) {
                history.addCurrentToHistory();
            }
            history.setIndex(-1);

            fireActionEvent(getInput());
        }
    }

    class NewDefaultKeyTypedAction extends ConsoleAction {
        private Action delegate = getKeymap().getDefaultAction();

        public NewDefaultKeyTypedAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            int inputStart = getInputStart();

            if(getCaretPosition() >= inputStart &&
               getSelectionStart() >= inputStart) {
                delegate.actionPerformed(evt);
            }
        }
    }

    class CaretBackwardAction extends ConsoleAction {
        private Action delegate = getActionMap().get(DefaultEditorKit.backwardAction);

        public CaretBackwardAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            int inputStart = getInputStart();

            if(getCaretPosition() != inputStart) {
                delegate.actionPerformed(evt);
            }
        }
    }

    class CaretBackwardSelectAction extends ConsoleAction {
        private Action delegate = getActionMap().get(DefaultEditorKit.selectionBackwardAction);

        public CaretBackwardSelectAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            int inputStart = getInputStart();

            if(getCaretPosition() != inputStart) {
                delegate.actionPerformed(evt);
            }
        }
    }

    class BeginLineAction extends ConsoleAction {
        private boolean select;

        public BeginLineAction(String name, KeyStroke keystroke, boolean select) {
            super(name, keystroke);
            this.select = select;
        }

        public void actionPerformed(ActionEvent e) {
            JTextComponent target = ConsolePanelBase.this;
            if (target != null) {
                try {
                    int offs = target.getCaretPosition();
                    int begOffs = Utilities.getRowStart(target, offs);
                    int input = getInputStart(); 

                    if (begOffs < input) {
                        begOffs = input;
                    }

                    if (select) {
                        target.moveCaretPosition(begOffs);
                    } else {
                        target.setCaretPosition(begOffs);
                    }
                } catch (BadLocationException bl) {
                    //UIManager.getLookAndFeel().provideErrorFeedback(
                    //        target);
                }
            }
        }
    }

    class PrevCommandAction extends ConsoleAction {
        private Action delegate = getActionMap().get(DefaultEditorKit.upAction);

        PrevCommandAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            if(getCaretPosition() >= getInputStart()) {
                history.historyPrevious();
            } else {
                delegate.actionPerformed(evt);
            }
        }
    }

    class NextCommandAction extends ConsoleAction {
        private Action delegate = getActionMap().get(DefaultEditorKit.downAction);

        NextCommandAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            if(getCaretPosition() >= getInputStart()) {
                history.historyNext();
            } else {
                delegate.actionPerformed(evt);
            }
        }
    }

    class SearchPrevAction extends ConsoleAction {
        public SearchPrevAction(String name, KeyStroke keystroke){
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            history.doBackwardSearch();
        }
    }

    class SearchNextAction extends ConsoleAction {
        public SearchNextAction(String name, KeyStroke keystroke){
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent evt) {
            history.doForwardSearch();
        }
    }

    class DummyAction extends ConsoleAction {
        public DummyAction(String name, KeyStroke keystroke){
            super(name, keystroke);
        }
        public void actionPerformed(ActionEvent evt) {
        }
    }

    class CopyAction extends ConsoleAction {
        Action delegate = getActionMap().get(DefaultEditorKit.copyAction);
        public CopyAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent event) {
            delegate.actionPerformed(event);
        }

        public boolean isEnabled() {
            Caret caret = getCaret();
            return caret.getMark() != caret.getDot();
        }
    }


    class CutAction extends ConsoleAction {
        Action copydelegate = getActionMap().get(DefaultEditorKit.copyAction);
        Action cutdelegate = getActionMap().get(DefaultEditorKit.cutAction);
        public CutAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent event) {
            Caret caret = getCaret();

            int start = Math.min(caret.getDot(), caret.getMark());

            if (start >= getInputStart()) {
                cutdelegate.actionPerformed(event);
            } else {
                copydelegate.actionPerformed(event);
            }
        }

        public boolean isEnabled() {
            Caret caret = getCaret();
            return caret.getMark() != caret.getDot();
        }
    }

    class PasteAction extends ConsoleAction {
        Action delegate = getActionMap().get(DefaultEditorKit.pasteAction);
        public PasteAction(String name, KeyStroke keystroke) {
            super(name, keystroke);
        }

        public void actionPerformed(ActionEvent event) {
            Caret caret = getCaret();

            int start = Math.min(caret.getDot(), caret.getMark());

            if (start >= getInputStart()) {
                delegate.actionPerformed(event);
            }
        }

        public boolean isEnabled() {
            return delegate.isEnabled();
        }
    }

    private HashMap<String, JMenuItem> menuItemsMap = new HashMap<String, JMenuItem>();
    private HashMap<String, ConsoleAction> consoleActions = new HashMap<String, ConsoleAction>();

    //   T E S T   G U I
    //

    static class TestConsoleThread extends Thread {
        ConsolePanelBase console;
        public TestConsoleThread(ConsolePanelBase console) {
            this.console = console;
        }
        public void run(){
            int i = 10;
            while(i-- > 0) {
                console.print("tread cnt="+i);
                try { Thread.sleep(1000); } catch (Exception ex) {}
            }
            console.printPrompt();
        }
    }

    static void setupGUI() {
        JFrame frame = new JFrame();

        final ConsolePanelBase console = new ConsolePanelBase();

        console.printPrompt();

        ActionListener listener = new ActionListener(){
            public void actionPerformed(ActionEvent actionEvent) {
                if (actionEvent.getActionCommand().equals("start")) {
                    console.print("started");

                    new TestConsoleThread(console).start();
                } else {
                    console.print("processing command "+actionEvent.getActionCommand()+"\nresult.. OK\n");
                    console.printPrompt();
                }
            }
        };

        console.addActionListener(listener);

        frame.add(new JScrollPane(console));
        frame.pack();
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);

    }

    public static void main(String[] args) {
        EventQueue.invokeLater(new Runnable(){
            public void run(){
                setupGUI();
            }
        });
    }


}

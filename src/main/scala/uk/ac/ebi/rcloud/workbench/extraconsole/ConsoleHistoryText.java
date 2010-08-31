package uk.ac.ebi.rcloud.workbench.extraconsole;

/**
 * User: andrew
 * Date: Sep 28, 2009
 */
public class ConsoleHistoryText extends ConsoleHistoryContainer {

    private ConsolePanelBase panel;

    public ConsoleHistoryText(ConsolePanelBase panel) {
        super(panel);
        this.panel = panel;
    }

    public int getInputStart() {
        return panel.getInputStart();
    }

    public String getText() {
        return panel.getInput();
    }

    public void setText(String text) {
        setIndex(-1);
        panel.setInput(text);
    }
}

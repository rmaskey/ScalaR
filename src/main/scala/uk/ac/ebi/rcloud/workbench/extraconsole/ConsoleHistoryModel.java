package uk.ac.ebi.rcloud.workbench.extraconsole;

import java.util.Vector;

/**
 * User: andrew
 * Date: Sep 28, 2009
 */
public class ConsoleHistoryModel {

    private Vector<String> data = new Vector<String>();

    public ConsoleHistoryModel() {
        this.data = new Vector<String>();
    }

    public void addItem(String item) {
        data.add(item);
    }

    public String getItem(int i) {
        return data.get(data.size() - 1 - i);
    }

    public void clear() {
        data.clear();
    }

    public int getSize() {
        return data.size();
    }

    public static void loadHistory() { }

    public static void saveHistory() { }

    public static void propertiesChanged() { }

}

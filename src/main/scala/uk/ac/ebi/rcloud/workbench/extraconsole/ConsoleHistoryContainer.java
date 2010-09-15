/*
 * Copyright (c) 2009-2010 European Molecular Biology Laboratory
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package uk.ac.ebi.rcloud.workbench.extraconsole;

import javax.swing.text.JTextComponent;
import javax.swing.text.Document;

/**
 * User: andrew
 * Date: Sep 28, 2009
 */
public class ConsoleHistoryContainer {

    private JTextComponent text;
    private ConsoleHistoryModel historyModel;
    private int index;
    private String current;

    public ConsoleHistoryContainer(JTextComponent text) {
        this.text = text;
        setModel(new ConsoleHistoryModel());
        index = -1;
    }

    public void fireActionPerformed() {
    }

    public int getIndex() {
        return index;
    }

    public void setIndex(int index) {
        this.index = index;
    }

    public ConsoleHistoryModel getModel() {
        return historyModel;
    }

    public void setModel(ConsoleHistoryModel model) {
        this.historyModel = model;
        index = -1;
    }

    public void addCurrentToHistory() {
        if(historyModel != null) {
            historyModel.addItem(getText());
        }
        index = 0;
    }

    public void doBackwardSearch() {
        if(historyModel == null)
            return;

        if(text.getSelectionEnd() != getDocument().getLength()) {
            text.setCaretPosition(getDocument().getLength());
        }

        int start = getInputStart();
        String t = getText().substring(0,
            text.getSelectionStart() - start);
        if(t == null) {
            historyPrevious();
            return;
        }

        int size = historyModel.getSize();
        int i = (index == size - 1) ? 0 : index + 1;

        for(int cnt = 0; cnt < size; cnt++) {
            String item = historyModel.getItem(i);
            if(item.startsWith(t)) {
                text.replaceSelection(item.substring(t.length()));
                text.select(getInputStart() + t.length(),
                    getDocument().getLength());
                index = i;
                return;
            }

            i = (i == size - 1) ? 0 : i + 1;
        }

        text.getToolkit().beep();
    }

    public void doForwardSearch() {
        if(historyModel == null)
            return;

        if(text.getSelectionEnd() != getDocument().getLength()) {
            text.setCaretPosition(getDocument().getLength());
        }

        int start = getInputStart();
        String t = getText().substring(0,
            text.getSelectionStart() - start);
        if(t == null) {
            historyNext();
            return;
        }

        int size = historyModel.getSize();
        int i = (index == 0) ? size - 1 : index - 1;

        for(int cnt = 0; cnt < size; cnt++) {
            String item = historyModel.getItem(i);
            if(item.startsWith(t)) {
                text.replaceSelection(item.substring(t.length()));
                text.select(getInputStart() + t.length(),
                    getDocument().getLength());
                index = i;
                return;
            }

            i = (i == 0) ? size - 1 : i - 1;
        }

        text.getToolkit().beep();
    }

    public void historyPrevious() {
        if(historyModel == null)
            return;

        if(index == historyModel.getSize() - 1)
            text.getToolkit().beep();
        else if(index == -1) {
            current = getText();
            setText(historyModel.getItem(0));
            index = 0;
        } else {
            // have to do this because setText() sets index to -1
            int newIndex = index + 1;
            setText(historyModel.getItem(newIndex));
            index = newIndex;
        }
    }

    public void historyNext() {
        if(historyModel == null)
            return;

        if(index == -1)
            text.getToolkit().beep();
        else if(index == 0)
            setText(current);
        else {
            // have to do this because setText() sets index to -1
            int newIndex = index - 1;
            setText(historyModel.getItem(newIndex));
            index = newIndex;
        }
    }

    public Document getDocument() {
        return text.getDocument();
    }

    public String getText() {
        return text.getText();
    }

    public void setText(String text) {
        this.index = -1;
        this.text.setText(text);
    }

    public int getInputStart() {
        return 0;
    }
}

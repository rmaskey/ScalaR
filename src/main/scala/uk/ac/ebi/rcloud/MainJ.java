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
package uk.ac.ebi.rcloud;

import uk.ac.ebi.rcloud.workbench.extraconsole.ConsolePanelBase;
import uk.ac.ebi.sr.RSession;
import uk.ac.ebi.sr.gui.RExecutor;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

/**
 * Date: Aug 3, 2010
 * @author Taalai Djumabaev
 */
public class MainJ {

    public static final JFrame frame = new JFrame();
    public static final String EMPTY = "";

    public static void main(String[] args) {
        final ConsolePanelBase console = new ConsolePanelBase();
        console.setPrompt("> ");
        console.printPrompt();

        final RExecutor exec = new RExecutor(RSession.currentSession().global());
        JScrollPane scroll = new JScrollPane(console);

        frame.setLayout(new BorderLayout());
//
//        ActionListener listener = new ActionListener() {
//            public void actionPerformed(ActionEvent event) {
//                JFileChooser chooser = new JFileChooser(".");
//                chooser.setFileSelectionMode(JFileChooser.DIRECTORIES_ONLY);
//                chooser.showDialog(frame, "click");
//                String[] res = chooser.getSelectedFile().list();
//
//                console.print(list2String(Arrays.asList(res)));
//            }
//
//            private String listToString(java.util.List<File> fs) {
//                StringBuilder sb = new StringBuilder();
//                for (File f : fs) {
//                    sb.append(f.toString());
//                    sb.append("\n");
//                }
//                return sb.toString();
//            }
//            private String list2String(java.util.List<String> fs) {
//                StringBuilder sb = new StringBuilder();
//                for (String f : fs) {
//                    sb.append(f);
//                    sb.append("\n");
//                }
//                return sb.toString();
//            }
//        };
        ActionListener listener = new ActionListener() {
            public void actionPerformed(ActionEvent event) {
                String input = event.getActionCommand().trim();
                if (input.equals(EMPTY)) {
                    console.setPrompt("> ");
                } else {
                    String res = exec.interpret(input);
                    console.print(res);
                    if (res.isEmpty()) console.setPrompt("> "); else console.setPrompt("\n> ");
                }
                console.printPrompt();
            }
        };

        console.addActionListener(listener);
        //console.setPrompt("R>");

        frame.add(scroll, BorderLayout.CENTER);

        frame.setSize(new Dimension(800, 600));
        frame.setLocationRelativeTo(null);
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setVisible(true);
    }


}

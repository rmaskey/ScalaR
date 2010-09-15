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

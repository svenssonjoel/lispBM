/*
  Copyright 2026 Joel Svensson  svenssonjoel@yahoo.se

  This program is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef QLBMCONTAINERWIDGET_H_
#define QLBMCONTAINERWIDGET_H_

#include "QLbmWidget.h"

class QBoxLayout;
class QGridLayout;

enum class QLbmLayout { VBox, HBox, Grid };

// A QLbmWidget that contains other QLbmWidgets arranged by a layout.
// Can be used both as the host-level root (via QLbmWidgetArea) and as a
// nestable container created from LispBM via qt-add-container.
class QLbmContainerWidget : public QLbmWidget {
  Q_OBJECT
public:
  explicit QLbmContainerWidget(QLbmLayout layout, QWidget *parent = nullptr);
  explicit QLbmContainerWidget(QWidget *parent = nullptr); // defaults to VBox

  void addChildWidget(QLbmWidget *child);
  void addChildWidgetAt(QLbmWidget *child, int row, int col); // grid only
  void removeChildWidget(QLbmWidget *child);

  QLbmLayout layoutType() const { return m_layoutType; }

private:
  QBoxLayout  *m_boxLayout  = nullptr;
  QGridLayout *m_gridLayout = nullptr;
  QLbmLayout   m_layoutType;
};

#endif

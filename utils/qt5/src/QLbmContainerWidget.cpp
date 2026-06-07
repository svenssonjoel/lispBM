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

#include "QLbmContainerWidget.h"

#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QGridLayout>
#include <QButtonGroup>

QLbmContainerWidget::QLbmContainerWidget(QWidget *parent)
  : QLbmContainerWidget(QLbmLayout::VBox, parent) {}

QLbmContainerWidget::QLbmContainerWidget(QLbmLayout layout, QWidget *parent)
  : QLbmWidget(parent)
  , m_layoutType(layout) {
  switch (layout) {
  case QLbmLayout::VBox:
    m_boxLayout = new QVBoxLayout(this);
    setLayout(m_boxLayout);
    break;
  case QLbmLayout::HBox:
    m_boxLayout = new QHBoxLayout(this);
    setLayout(m_boxLayout);
    break;
  case QLbmLayout::Grid:
    m_gridLayout = new QGridLayout(this);
    setLayout(m_gridLayout);
    break;
  }
}

void QLbmContainerWidget::addChildWidget(QLbmWidget *child) {
  if (m_boxLayout) {
    m_boxLayout->addWidget(child);
  } else if (m_gridLayout) {
    m_gridLayout->addWidget(child, m_gridLayout->rowCount(), 0);
  }
}

void QLbmContainerWidget::addChildWidgetAt(QLbmWidget *child, int row, int col) {
  if (m_gridLayout) {
    m_gridLayout->addWidget(child, row, col);
  }
}

QButtonGroup *QLbmContainerWidget::radioGroup() {
  if (!m_radioGroup)
    m_radioGroup = new QButtonGroup(this);
  return m_radioGroup;
}

void QLbmContainerWidget::addExternalWidget(QWidget *child) {
  if (m_boxLayout)
    m_boxLayout->addWidget(child);
  else if (m_gridLayout)
    m_gridLayout->addWidget(child, m_gridLayout->rowCount(), 0);
}

void QLbmContainerWidget::removeChildWidget(QLbmWidget *child) {
  if (m_boxLayout)
    m_boxLayout->removeWidget(child);
  else if (m_gridLayout)
    m_gridLayout->removeWidget(child);
  child->deleteLater();
}

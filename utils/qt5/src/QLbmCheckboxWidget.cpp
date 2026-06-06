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

#include "QLbmCheckboxWidget.h"

#include <QCheckBox>
#include <QVBoxLayout>

QLbmCheckboxWidget::QLbmCheckboxWidget(const QString &label, int handle, QWidget *parent)
  : QLbmWidget(parent)
  , m_handle(handle) {
  auto *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  m_checkbox = new QCheckBox(label, this);
  layout->addWidget(m_checkbox);
  setLayout(layout);
  connect(m_checkbox, &QCheckBox::toggled, this, [this](bool checked) {
    emit changed(m_handle, checked);
  });
}

bool QLbmCheckboxWidget::getValue() const { return m_checkbox->isChecked(); }
void QLbmCheckboxWidget::setValue(bool checked) { m_checkbox->setChecked(checked); }

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

#include "QLbmRadioWidget.h"

#include <QRadioButton>
#include <QVBoxLayout>

QLbmRadioWidget::QLbmRadioWidget(const QString &label, int handle, QWidget *parent)
  : QLbmWidget(parent)
  , m_handle(handle) {
  auto *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  m_radio = new QRadioButton(label, this);
  layout->addWidget(m_radio);
  setLayout(layout);
  connect(m_radio, &QRadioButton::toggled, this, [this](bool checked) {
    if (checked) emit selected(m_handle);
  });
}

bool QLbmRadioWidget::getValue() const { return m_radio->isChecked(); }
void QLbmRadioWidget::setValue(bool checked) { m_radio->setChecked(checked); }

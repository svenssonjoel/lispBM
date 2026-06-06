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

#include "QLbmButtonWidget.h"

#include <QPushButton>
#include <QVBoxLayout>

QLbmButtonWidget::QLbmButtonWidget(const QString &label, int handle, QWidget *parent)
  : QLbmWidget(parent)
  , m_handle(handle) {
  auto *layout = new QVBoxLayout(this);
  layout->setContentsMargins(0, 0, 0, 0);
  m_btn = new QPushButton(label, this);
  layout->addWidget(m_btn);
  setLayout(layout);
  connect(m_btn, &QPushButton::clicked, this, [this]() {
    emit clicked(m_handle);
  });
}

void QLbmButtonWidget::setLabel(const QString &label) {
  m_btn->setText(label);
}
